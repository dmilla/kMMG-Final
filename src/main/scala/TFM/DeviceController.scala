package TFM

import java.nio.{ByteBuffer, ByteOrder}

import TFM.CommProtocol.{ConnectToDeviceRequest, UpdateCoords}
import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.io.IO
import akka.stream.actor.ActorPublisher
import akka.stream.{ActorMaterializer, Inlet}
import akka.stream.scaladsl._
import akka.util.ByteString
import com.github.jodersky.flow.{Parity, SerialSettings}
import com.github.jodersky.flow.stream.Serial

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import com.github.jodersky.flow.{AccessDeniedException, SerialSettings}

/**
  * Created by diego on 9/05/16.
  */
class DeviceController extends Actor{

  //var PORT = "/dev/ttyUSB0"
  val BAUD_RATE = 115200
  val CHARACTER_SIZE = 8
  val PARITY = Parity.None
  val TWO_STOP_BITS = false
  val DEVICE_SETTINGS = SerialSettings(BAUD_RATE, CHARACTER_SIZE, TWO_STOP_BITS, PARITY)
  val BUFFER_SIZE = 4
  val SENSOR_STEPS = 4096
  val DEGREES_PER_STEP = SENSOR_STEPS / 360
  val DISTANCE_FROM_CENTER_TO_MOTORS = 3.0
  val ARM_LENGTH_1 = 27.0
  val ARM_LENGTH_2 = 30.0

  var initialCoords: (Short, Short) = (-1, -1)
  var lastSensorValues: (Short, Short) = (-1, -1)

 // val Delay = FiniteDuration(1000, MILLISECONDS)

  implicit val system = kMMGUI.controlSystem

  val bytePublisherRef = system.actorOf(Props[BytePublisher])
  val bytePublisher = ActorPublisher[ByteString](bytePublisherRef)

  var badResponses = 0

  def connectToDeviceStream(port: String) = {
    import system.dispatcher
    implicit val materializer = ActorMaterializer()

    notify("intentando conectar a " +  port)

    val serial: Flow[ByteString, ByteString, Future[Serial.Connection]] =
      Serial().open(port, DEVICE_SETTINGS, false, BUFFER_SIZE)

    val printer: Sink[ByteString, _] = Sink.foreach[ByteString]{data =>
      notify("device says bulk: " + data.toString)
      if (data.size == 4) {
        badResponses = 0
        val shorts = convert(data.seq)
        if (shorts.size == 2) {
          lastSensorValues = (shorts(0), shorts(1))
          notify("device says (shorts): " + shorts(0) + " / " + shorts(1))
          if (initialCoords ==(-1, -1)) initialCoords = (shorts(0), shorts(1))
          else getCoordinates((shorts(0), shorts(1)), initialCoords)
        }
        bytePublisherRef ! Publish(ByteString(-0.toByte, 0.toByte, 0.toByte, 0.toByte)) // TODO - Send last calculated force
      } else {
        badResponses += 1
        notify("Less than 4 bytes received from device! Consecutive times: " + badResponses)
        if (badResponses > 2) reconnect(materializer, port)
      }
    }

    val source: Source[ByteString, _] = Source.fromPublisher(bytePublisher)

    //Working test source
    /*val ticker: Source[ByteString, _] = Source.tick(Delay, Delay, ()).scan(0){case (x, _) =>
      sendForce //TEST - NOT IMPLEMENTED YET
    }.map{ x =>
      notify("Sending (0 , 0) to motors")
      ByteString(-0.toByte, 0.toByte, 0.toByte, 0.toByte)
    }*/

    source.viaMat(serial)(Keep.right).to(printer).run()  onComplete {
      case Success(connection) => {
        notify("succesfully connected! " + connection)
      }
      case Failure(error) => notify("error trying to connect: " + error)
    }
  }

  def reconnect(materializer: ActorMaterializer, port: String): Boolean = {
    materializer.shutdown()
    connectToDeviceStream(port)
    true
  }

  /*def connectToDevice(port: String) = {
    notify("intentando conectar a " +  port)
    IO(Serial) ! Serial.Open(port, DEVICE_SETTINGS)
  }*/

  def getCoordinates(currentValues: (Short, Short), initialValues: (Short, Short)) = {
    val degAngle1 = 90.0f + (currentValues._1 - initialValues._1) / DEGREES_PER_STEP
    val degAngle2 = -(currentValues._2 - initialValues._2) / DEGREES_PER_STEP
    val angle1 = Math.toRadians(degAngle1)
    val angle2 = Math.toRadians(degAngle2)

    // Those are the coordinates of the two axes (the ends of the first arms)
    // ARM_LENGHT_1 is the hypotenuse of the triangle formed by the coords
    //
    // Take into account that the coordinates are from the motors position,
    // and y is going down
    val x1 = - DISTANCE_FROM_CENTER_TO_MOTORS - ARM_LENGTH_1 * Math.cos(angle1)
    val y1 = - ARM_LENGTH_1 * Math.sin(angle1)

    val x2 = DISTANCE_FROM_CENTER_TO_MOTORS + ARM_LENGTH_1 * Math.cos(angle2)
    val y2 = - ARM_LENGTH_1 * Math.sin(angle2)

    // This is the distance between the point (x1, y1) and the point (x2, y2)
    val r = Math.sqrt( Math.pow(x2-x1, 2) + Math.pow(y2-y1, 2) )

    // With that distance we can calculate alpha, which is the angle between
    // r and the second arm
    val cosAlpha = r / (2 * ARM_LENGTH_2)
    val alpha = Math.acos(cosAlpha)

    // This angle is the angle between r and the normal
    // doing so, with alpha and theta, we can obtain
    // a rectangle triangle to calculate px and py
    val theta = Math.atan( (y2-y1) / (x2-x1) )

    // Now we can know the absolute coordinates using that triangle
    val px = x1 + ARM_LENGTH_2 * Math.cos(alpha - theta)
    val py = y1 - ARM_LENGTH_2 * Math.sin(alpha - theta)

    val normalizedCoords = normalizeCoords(px, py)
    notify("Coordinates calculated! X: " + px + " - Y: " + py + " // Normalized values: " + normalizedCoords)
    kMMGUI.conductor ! UpdateCoords(normalizedCoords)
    kMMGUI.joystickChart ! UpdateCoords(normalizedCoords)
  }

  //TODO verify X axis is between -48/48
  //This method normalizes coords between 0 and 1
  def normalizeCoords(x: Double, y: Double): (Double, Double) = {
    val normX = (x + 48)/96 // Device X axis is between -48 and 48
    val normY = (y + 56.85)/56.85 // Device Y axis is between -56.85 and 0
    (normX, normY)
  }

  // TODO - remove method if stream working properly
  def sendForce= {
    if (true) 0
    else 0
  }

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def bytesToShort(byte1: Byte, byte2: Byte) : Short = {
    ByteBuffer
      .allocate(2)
      .order(ByteOrder.LITTLE_ENDIAN)
      .put(byte1)
      .put(byte2)
      .getShort(0)
  }

  def convert(in: IndexedSeq[Byte]): Array[Short] =
    in.grouped(2).map { case IndexedSeq(hi, lo) => bytesToShort(hi, lo)} .toArray

  def receive: Receive = {
    case ConnectToDeviceRequest(port) => connectToDeviceStream(port)
    /*case Serial.Received(data) => {
      println("Received data: " + data.toString)
    }
    case Serial.CommandFailed(cmd: Serial.Open, reason: AccessDeniedException) =>
      notify("You're not allowed to open that port!")
    case Serial.CommandFailed(cmd: Serial.Open, reason) =>
      notify("Could not open port for some other reason: " + reason.getMessage)
    case Serial.Opened(settings) => {
      notify("Connection Succesful!")
      val operator = sender
      operator ! Serial.Write(ByteString(0.toByte, 0.toByte,0.toByte,0.toByte))
      //do stuff with the operator, e.g. context become opened(op)
    }*/
    case _ ⇒ println("DeviceController received unknown message")
  }
}