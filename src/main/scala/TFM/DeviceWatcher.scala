package TFM

/**
  * Created by diego on 20/05/16.
  */
import TFM.CommProtocol.ConnectToDeviceRequest
import akka.actor.{Actor, ActorLogging}
import akka.io.IO
import com.github.jodersky.flow.Serial


class DeviceWatcher extends Actor with ActorLogging {
  import context._

  val ports = List(
    "/dev/ttyUSB\\d+",
    "/dev/ttyACM\\d+",
    "/dev/cu\\d+",
    "/dev/ttyS\\d+"
  )

  override def preStart() = {
    val cmd = Serial.Watch("/dev")
    IO(Serial) ! cmd //watch for new devices
    //notify(s"Watching ${cmd.directory} for new devices.")
    log.info(s"Watching ${cmd.directory} for new devices.")
  }

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def receive = {

    case Serial.CommandFailed(w: Serial.Watch, err) =>
      //notify(s"Could not get a watch on ${w.directory}. " + err)
      log.error(err, s"Could not get a watch on ${w.directory}.")
      context stop self

    case Serial.Connected(path) =>
      //notify(s"New device: ${path}")
      log.info(s"New device: ${path}")
      ports.find(path matches _) match {
        case Some(port) => {
          //notify(s"Device is a serial device.")
          log.info(s"Device is a serial device.")
          if (path == "/dev/ttyUSB0") kMMGUI.deviceController ! ConnectToDeviceRequest(path)
        }
        case None => {
          //notify(s"Device is NOT serial device.")
          log.warning(s"Device is NOT serial device.")
        }
      }

  }

}