package TFM

import javax.sound.midi._

import TFM.CommProtocol._
import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.Await


/**
  * Created by diego on 31/05/16.
  */


class Conductor extends Actor{

  val sequencer = MidiSystem.getSequencer
  val sequence = new Sequence(Sequence.PPQ, 4)
  val track = sequence.createTrack()

  var lastState: (Int, Int) = (0, 4)
  var lastCoords: (Double, Double) = (0.5, 0.5)
  var lastNoteCoords: (Double, Double) = (0.5, 0.5)

  var outNormalization: Byte = 36

  var initialized: Boolean = false
  var started: Boolean = false

  var currentTick: Long = 0
  var currentNoteEndTick: Long = 0

  val sequencerSystem = ActorSystem("sequencerSystem")
  val sequencerWatcher = sequencerSystem.actorOf(Props(classOf[SequencerWatcher], sequencer))
  var tickChecker = sequencerSystem.scheduler.schedule(1 milliseconds, 1 millisecond)(sequencerWatcher ! CheckSequencerTick)

  def initializeSequencer = {
    if (!initialized) {
      sequencer.open()
      sequencer.setSequence(sequence)
      sequencer.setTempoInBPM(120)// TODO variable tempo add GUI Fields
      track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, outNormalization, 127), 1))
      track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, outNormalization, 127), 5))
      currentNoteEndTick = 5
    }
    true
  }

  def startMelodyGeneration = {
    if (!started) {
      initializeSequencer
      sequencer.setTickPosition(0)
      sequencer.start()
      started = true
    } else {
      notify("Melody generation already started")
    }
  }

  def stopMelodyGeneration = {
    if (started) {
      sequencer.stop()
      started = false
    } else {
      notify("Melody generation already stopped")
    }
  }

  def updateSequencerTick(tick: Long) = {
    notify("New Sequencer tick reached! " + tick)
    currentTick = tick
    if (currentTick >= currentNoteEndTick - 1) getNextNote
    else if (lastCoords._2 < lastNoteCoords._2) { // TODO improve to cut note only if more than control duration elapsed since last note started
      currentNoteEndTick = tick + 1
      track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, lastState._1 + outNormalization, 127), currentNoteEndTick))
      getNextNote
    }
  }

  def addNextNote(note: (Int, Int)) = {
    track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, note._1 + outNormalization, 127), currentNoteEndTick + 1))
    val endTick = currentNoteEndTick + 1 + note._2
    track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, note._1 + outNormalization, 127), endTick))
    currentNoteEndTick = endTick
    lastState = note
    lastNoteCoords = lastCoords
  }

  def getNextNote = {
    implicit val timeout = Timeout(10 milliseconds)
    val listFuture = kMMGUI.markovExtractor ? TransitionsRequest(lastState)
    val transitions = Await.result(listFuture, timeout.duration).asInstanceOf[List[((Int, Int), Double)]] // Warning, synchronous request!
    if (transitions.nonEmpty) {
      val noteFuture = kMMGUI.controller ? CalcNoteOutputRequest(transitions, lastCoords._1, lastCoords._2)
      val nextNote = Await.result(noteFuture, timeout.duration).asInstanceOf[(Int, Int)]
      addNextNote(nextNote)
    }
    else {
      stopMelodyGeneration
      notify("Error while trying to get Markov Transitions, please generate model first. Melody generation has been stopped.")
    }
  }

  /*def sendNextNote = {
    val coords = getLastCoords
    implicit val timeout = Timeout(2 seconds)
    val transitions: Future[List[(Int, Double)]] = ask(kMMGUI.markovExtractor, TransitionsRequest(lastState)).mapTo[List[(Int, Double)]]
    transitions.onSuccess{
      case list: List[((Int, Int), Double)] =>
        if (list.nonEmpty) kMMGUI.controller ! CalcNoteOutputRequest(list, coords._1, coords._2)
        else notify("Error while trying to get Markov Transitions, please generate model first")
    }
  }

  def getLastCoords = {
    (JoystickChart.lastX, JoystickChart.lastY)
  }*/

  /*def addMetaEventListener(listener: MetaEventListener): Boolean = false

  def getMaxReceivers: Int = -1*/

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def receive: Receive = {
    case StartMelodyGenerationRequest => startMelodyGeneration
    case NewSequencerTick(tick: Long) => updateSequencerTick(tick)
    case UpdateCoords(coords) => lastCoords = coords
    //case NotifyNoteFinished => sendNextNote
    //case UpdateStatus(status: (Int, Int)) => lastState = status
    case _ ⇒ println("Conductor received unknown message")
  }

}
