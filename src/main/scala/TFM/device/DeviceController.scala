package TFM.device

import java.nio.{ByteBuffer, ByteOrder}

import TFM.CommProtocol.{ConnectToDeviceRequest, EndManualControlRequest, UpdateCoords, UpdateFeedbackForce}
import TFM.kMarkovMelodyGenerator.kMMGUI
import akka.actor.{Actor, Props}
import akka.stream.ActorMaterializer
import akka.stream.actor.ActorPublisher
import akka.stream.scaladsl._
import akka.util.ByteString
import com.github.jodersky.flow.{Parity, SerialSettings}
import com.github.jodersky.flow.stream.Serial

import scala.concurrent.Future
import scala.util.{Failure, Success}

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
  val DEGREES_PER_STEP: Float = SENSOR_STEPS / 360f
  val DISTANCE_FROM_CENTER_TO_MOTORS: Float = 3.0f
  val ARM_LENGTH_1: Float = 27.0f
  val ARM_LENGTH_2: Float = 30.0f

  var initialSensorValues: (Short, Short) = (-1, -1)
  var currentCoords: (Float, Float) = (-1, -1)
  var lastArmsAngles: (Float, Float) = (-1, -1)
  val jacobian = Array.ofDim[Float](2,2)


 // val Delay = FiniteDuration(1000, MILLISECONDS)

  implicit val system = kMMGUI.controlSystem

  val bytePublisherRef = system.actorOf(Props[BytePublisher])
  val bytePublisher = ActorPublisher[ByteString](bytePublisherRef)

  //var deviceTorque = ByteString(-32, -4, 32, 3)
  var deviceTorque = ByteString(0, 0, 0, 0)
  var badResponses = 0
  var calibrated = false

  def connectToDeviceStream(port: String) = {
    import system.dispatcher
    implicit val materializer = ActorMaterializer()

    notify("intentando conectar a " +  port)

    val serial: Flow[ByteString, ByteString, Future[Serial.Connection]] =
      //Serial().open(port, DEVICE_SETTINGS, false, BUFFER_SIZE) // TODO - try stream connection without specific buffer size
      Serial().open(port, DEVICE_SETTINGS)

    val printer: Sink[ByteString, _] = Sink.foreach[ByteString]{data =>
      notify("device says bulk: " + data.toString)
      if (data.size == 4) {
        badResponses = 0
        val shorts = convert(data.seq)
        if (shorts.size == 2) { // TODO - negative values = 4096 - value????
          notify("device says (shorts): " + shorts(0) + " / " + shorts(1))
          if (initialSensorValues ==(-1, -1) || (calibrated == false && initialSensorValues != (shorts(0), shorts(1)))) {
            initialSensorValues = (shorts(0), shorts(1))
            calibrated = true
          }
          else getCoordinates((shorts(0), shorts(1)), initialSensorValues)
        }
        bytePublisherRef ! Publish(deviceTorque)
      } else {
        badResponses += 1
        notify("Less than 4 bytes received from device! Consecutive times: " + badResponses)
        bytePublisherRef ! Publish(deviceTorque)
      }
    }

    val source: Source[ByteString, _] = Source.fromPublisher(bytePublisher)

    source.viaMat(serial)(Keep.right).to(printer).run()  onComplete {
      case Success(connection) => {
        notify("succesfully connected! " + connection)
        kMMGUI.joystickChart ! EndManualControlRequest
        bytePublisherRef ! Publish(deviceTorque)
      }
      case Failure(error) => notify("error trying to connect: " + error)
    }
  }

  /*def connectToDevice(port: String) = {
    notify("intentando conectar a " +  port)
    IO(Serial) ! Serial.Open(port, DEVICE_SETTINGS)
  }*/

  def getCoordinates(currentValues: (Short, Short), initialValues: (Short, Short)) = {
    val degAngle1: Float = 90.0f + (currentValues._1 - initialValues._1) / DEGREES_PER_STEP
    val degAngle2: Float = -(currentValues._2 - initialValues._2) / DEGREES_PER_STEP
    val angle1: Float = Math.toRadians(degAngle1).toFloat
    val angle2: Float = Math.toRadians(degAngle2).toFloat

    lastArmsAngles = (angle1, angle2)

    // Those are the coordinates of the two axes (the ends of the first arms)
    // ARM_LENGHT_1 is the hypotenuse of the triangle formed by the coords
    //
    // Take into account that the coordinates are from the motors position,
    // and y is going down
    val x1: Float = - DISTANCE_FROM_CENTER_TO_MOTORS - ARM_LENGTH_1 * Math.cos(angle1).toFloat
    val y1: Float = - ARM_LENGTH_1 * Math.sin(angle1).toFloat

    val x2: Float = DISTANCE_FROM_CENTER_TO_MOTORS + ARM_LENGTH_1 * Math.cos(angle2).toFloat
    val y2: Float = - ARM_LENGTH_1 * Math.sin(angle2).toFloat

    // This is the distance between the point (x1, y1) and the point (x2, y2)
    val r: Float = Math.sqrt( Math.pow(x2-x1, 2) + Math.pow(y2-y1, 2) ).toFloat

    // With that distance we can calculate alpha, which is the angle between
    // r and the second arm
    val cosAlpha: Float = r / (2 * ARM_LENGTH_2)
    val alpha: Float = Math.acos(cosAlpha).toFloat

    // This angle is the angle between r and the normal
    // doing so, with alpha and theta, we can obtain
    // a rectangle triangle to calculate px and py
    val theta: Float = Math.atan( (y2-y1) / (x2-x1) ).toFloat

    // Now we can know the absolute coordinates using that triangle
    val px: Float = x1 + ARM_LENGTH_2 * Math.cos(alpha - theta).toFloat
    val py: Float = y1 - ARM_LENGTH_2 * Math.sin(alpha - theta).toFloat
    currentCoords = (px, py)

    // Vector
    // dx1
    jacobian(0)(0) = (px + DISTANCE_FROM_CENTER_TO_MOTORS + ARM_LENGTH_1 * Math.cos(angle1)).toFloat
    // dx2
    jacobian(0)(1) = (px - DISTANCE_FROM_CENTER_TO_MOTORS - ARM_LENGTH_1 * Math.cos(angle2)).toFloat
    // dy1
    jacobian(1)(0) = (py + ARM_LENGTH_1 * Math.sin(angle1)).toFloat
    // dy2
    jacobian(1)(1) = (py + ARM_LENGTH_1 * Math.sin(angle2)).toFloat

    val normalizedCoords = normalizeCoords(px, py)
    notify("Coordinates calculated! X: " + px + " - Y: " + py + " // Normalized values: " + normalizedCoords)
    kMMGUI.conductor ! UpdateCoords(normalizedCoords)
    kMMGUI.joystickChart ! UpdateCoords(normalizedCoords)
  }

  //TODO verify X axis is between -48/48 and reduce total output (corners can't be reached!)
  //This method normalizes coords between 0 and 1
  def normalizeCoords(x: Double, y: Double): (Double, Double) = {
    val normX = (x + 48)/96 // Device X axis is between -48 and 48
    val normY = (y + 56.85)/56.85 // Device Y axis is between -56.85 and 0
    (normX, normY)
  }

  def calcVirtualForce(forceVector: (Float, Float)): ByteString = {
    val ddd: Float = jacobian(0)(0) * jacobian(1)(1) - jacobian(0)(1) * jacobian(1)(0)

    val d11: Float = ((currentCoords._1 + DISTANCE_FROM_CENTER_TO_MOTORS) * ARM_LENGTH_1 * Math.sin(lastArmsAngles._1) - currentCoords._1 * ARM_LENGTH_1 * Math.cos(lastArmsAngles._1)).toFloat
    val d12: Float = 0.0f
    val d21: Float = 0.0f
    val d22: Float = (-(currentCoords._1 + DISTANCE_FROM_CENTER_TO_MOTORS) * ARM_LENGTH_1 * Math.sin(lastArmsAngles._2) - currentCoords._1 * ARM_LENGTH_1 * Math.cos(lastArmsAngles._2)).toFloat


    var tau1: Float = (( jacobian(1)(1) * d11 - jacobian(0)(1) * d21) * forceVector._1 + ( jacobian(1)(1) * d12 - jacobian(0)(1) * d22) * forceVector._2) / ddd
    var tau2: Float = ((-jacobian(1)(0) * d11 + jacobian(0)(0) + d21) * forceVector._1 + (-jacobian(1)(0) * d12 + jacobian(0)(0) * d22) * forceVector._2) / ddd

    tau1 = Math.max(tau1, -20000.0f)
    tau1 = Math.min(tau1, 20000.0f)

    tau2 = Math.max(tau2, -20000.0f)
    tau2 = Math.min(tau2, 20000.0f)

    //out[0] = -tau1;
    //out[1] = tau2;
    //notify(-tau1 + " " + tau2)
    val res = ByteString(shortToBytes((-tau1).toShort) ++ shortToBytes(tau2.toShort))
    deviceTorque = res
    res
  }

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def bytesToShort(byte1: Byte, byte2: Byte): Short = {
    ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN).put(byte1).put(byte2).getShort(0)
  }

  def shortToBytes(short: Short) = {
    ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN).putShort(short).array
  }

  def convert(in: IndexedSeq[Byte]): Array[Short] =
    in.grouped(2).map { case IndexedSeq(hi, lo) => bytesToShort(hi, lo)} .toArray

  def receive: Receive = {
    case ConnectToDeviceRequest(port) => connectToDeviceStream(port)
    case UpdateFeedbackForce(forceVector: (Float, Float)) => calcVirtualForce(forceVector)
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