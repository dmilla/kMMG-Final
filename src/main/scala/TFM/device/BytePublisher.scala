package TFM.device

import TFM.kMarkovMelodyGenerator.kMMGUI
import akka.stream.actor.ActorPublisher
import akka.stream.actor.ActorPublisherMessage.{Cancel, Request}
import akka.util.ByteString

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


/**
  * Created by root on 27/05/16.
  */
class BytePublisher extends ActorPublisher[ByteString] {
  var queue: mutable.Queue[ByteString] = mutable.Queue()
  var tickChecker = kMMGUI.controlSystem.scheduler.schedule(10 milliseconds, 10 milliseconds)(publishIfNeeded())

  def receive = {
    case Publish(b: ByteString) =>
      if (queue.isEmpty && b.size == 4) queue.enqueue(b)
      else if (queue.isEmpty) queue.enqueue(ByteString(0.toByte, 0.toByte, 0.toByte, 0.toByte))
      else notify("BytePublisher received another request but queue is already full")
      //publishIfNeeded()
    case Request(cnt) =>
      publishIfNeeded()
    case Cancel => context.stop(self)
    case _ => notify("BytePublisher received unknown message")
  }

  def publishIfNeeded() = {
    while (queue.nonEmpty && isActive && totalDemand > 0) {
      notify("\nforce sent! " + queue(0) + "\n")
      onNext(queue.dequeue())
    }
  }

  def notify(msg: String) = kMMGUI.addOutput(msg) // TODO - notify or simply println for dev msgs?
}

case class Publish[D](data: D)