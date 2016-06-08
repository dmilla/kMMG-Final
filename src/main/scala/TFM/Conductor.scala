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

// TODO - add option to save midi track
class Conductor extends Actor{

  val durations = List(1, 2, 3, 4, 6, 8, 12, 16)

  val sequencer = MidiSystem.getSequencer
  val sequence = new Sequence(Sequence.PPQ, 4)
  val track = sequence.createTrack()

  var currentState: (Int, Int) = (0, 4)
  var currentStateTransitions: List[((Int, Int), Double)] = List.empty[((Int, Int), Double)]
  var currentCoords: (Double, Double) = (0.5, 0.5)
  var lastNoteCoords: (Double, Double) = (0.5, 0.5)

  var outNormalization: Byte = 36 // TODO add output normalization to GUI

  var initialized: Boolean = false
  var started: Boolean = false

  var currentTick: Long = 0
  //var currentNoteEndTick: Long = 0
  var currentNoteEndMidiEvent = new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, outNormalization, 127), 5)

  val sequencerSystem = ActorSystem("sequencerSystem")
  val sequencerWatcher = sequencerSystem.actorOf(Props(classOf[SequencerWatcher], sequencer))
  var tickChecker = sequencerSystem.scheduler.schedule(1 milliseconds, 1 millisecond)(sequencerWatcher ! CheckSequencerTick)

  def initializeSequencer = {
    if (!initialized) {
      sequencer.open()
      sequencer.setSequence(sequence)
      sequencer.setTempoInBPM(120)// TODO variable tempo add GUI Fields
      track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, outNormalization, 127), 1))
      track.add(currentNoteEndMidiEvent)
      kMMGUI.markovExtractor ! TransitionsRequest(currentState)
    }
    true
  }

  def startMelodyGeneration = {
    if (!started) {
      initializeSequencer
      //sequencer.setTickPosition(0)
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
    if (currentTick >= currentNoteEndMidiEvent.getTick- 1) getNextNote
    else if (currentCoords._1 < lastNoteCoords._1) {
      val controlDurationIndex = (7 * currentCoords._1).round.toInt // Normalized to 8 possible durations
      val controlDuration = durations(controlDurationIndex)
      val currentNoteElapsed = currentTick - (currentNoteEndMidiEvent.getTick - currentState._2)
      if (currentNoteElapsed > controlDuration) {
        track.remove(currentNoteEndMidiEvent)
        val currentNoteEndTick = currentTick + 1
        currentNoteEndMidiEvent = new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, currentState._1 + outNormalization, 127), currentNoteEndTick)
        track.add(currentNoteEndMidiEvent)
        getNextNote
      }
    }
  }

  def addNextNote(note: (Int, Int)) = {
    val currentNoteEndTick = currentNoteEndMidiEvent.getTick
    track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, note._1 + outNormalization, 127), currentNoteEndTick + 1))
    val endTick = currentNoteEndTick + 1 + note._2
    currentNoteEndMidiEvent = new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, note._1 + outNormalization, 127), endTick)
    track.add(currentNoteEndMidiEvent)
    currentState = note
    kMMGUI.markovExtractor ! TransitionsRequest(currentState)
    lastNoteCoords = currentCoords
  }

  def getNextNote = {
    if (currentStateTransitions.nonEmpty) {
      implicit val timeout = Timeout(20 milliseconds)
      val noteFuture = kMMGUI.kController ? CalcNoteOutputRequest(currentStateTransitions, currentCoords._1, currentCoords._2)
      val nextNote = Await.result(noteFuture, timeout.duration).asInstanceOf[(Int, Int)] // Warning, synchronous request!
      addNextNote(nextNote)
    }
    else {
      stopMelodyGeneration
      notify("Error while trying to get Markov Transitions, please generate model first. Melody generation has been stopped.")
    }
  }

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def receive: Receive = {
    case StartMelodyGenerationRequest => startMelodyGeneration
    case StopMelodyGenerationRequest => stopMelodyGeneration
    case NewSequencerTick(tick: Long) => updateSequencerTick(tick)
    case UpdateCoords(coords) => currentCoords = coords
    case TransitionsList(list: List[((Int, Int), Double)]) => {
      currentStateTransitions = list
      notify("\nConductor received new transitions list!!!\n")
    }
    case _ ⇒ println("Conductor received unknown message")
  }

  /*CONNECT TO CUSTOM MIDI DEVICE
  val devices = MidiSystem.getMidiDeviceInfo().map(MidiSystem.getMidiDevice)

  // Print and choose one device which you want to use.
  devices.zipWithIndex.foreach { case (d, i) => println(s"${i} ${d.getDeviceInfo} (${d.getClass.getSimpleName})") }

  val dev = devices(0)

  dev.open()
  val rcvr = dev.getReceiver()

  val msg = new ShortMessage
  msg.setMessage(NOTE_ON, 0, 60, 93)

  rcvr.send(msg, -1)

  dev.close()
   */

}
