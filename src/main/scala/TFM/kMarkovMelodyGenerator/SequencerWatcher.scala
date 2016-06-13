package TFM.kMarkovMelodyGenerator

/**
  * Created by diego on 6/06/16.
  */

import javax.sound.midi._

import TFM.CommProtocol._
import akka.actor.Actor

class SequencerWatcher(sequencer: Sequencer) extends Actor{

  var lastTick: Long = 0

  def checkSequencerTick = {
    if (sequencer.isRunning) {
      //kMMGUI.addOutput("checking tick")
      val tick = sequencer.getTickPosition
      if (lastTick != tick) {
        kMMGUI.conductor ! NewSequencerTick(tick)
        lastTick = tick
      }
    }
  }

  def receive: Receive = {
    case CheckSequencerTick => checkSequencerTick
    case _ ⇒ println("Sequencer Watcher received unknown message")
  }

}
