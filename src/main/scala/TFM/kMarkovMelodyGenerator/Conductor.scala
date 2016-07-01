package TFM.kMarkovMelodyGenerator

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import javax.sound.midi._

import TFM.CommProtocol._
import TFM.util.FixedList
import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


/**
  * Created by diego on 31/05/16.
  */

class Conductor extends Actor{

  val durations = List(1, 2, 3, 4, 6, 8, 12, 16)

  val sequencer = MidiSystem.getSequencer
  val sequence = new Sequence(Sequence.PPQ, 4)
  val track = sequence.createTrack()

  var currentState: FixedList[(Int, Int)] = new FixedList[(Int, Int)](3)
  var currentStateTransitions: List[((Int, Int), Double)] = List.empty[((Int, Int), Double)]
  var currentCoords: (Double, Double) = (0.5, 0.5)
  var lastNoteCoords: (Double, Double) = (0.5, 0.5)

  var outNormalization: Byte = 48
  var currentTempo: Int = 120
  var programChange: Byte = 0

  var initialized: Boolean = false
  var started: Boolean = false

  var currentTick: Long = 0
  //var currentNoteEndTick: Long = 0
  var currentNoteEndMidiEvent = new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, 12 + outNormalization, 127), 5)

  val sequencerSystem = ActorSystem("sequencerSystem")
  val sequencerWatcher = sequencerSystem.actorOf(Props(classOf[SequencerWatcher], sequencer))
  var tickChecker = sequencerSystem.scheduler.schedule(1 milliseconds, 1 millisecond)(sequencerWatcher ! CheckSequencerTick)

  def initializeSequencer = {
    if (!initialized && currentState.full && !currentStateTransitions.isEmpty) {
      sequencer.open()
      sequencer.setSequence(sequence)
      sequencer.setTempoInBPM(currentTempo)
      //track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, 12 + outNormalization, 127), 1))
      //track.add(currentNoteEndMidiEvent)
      //kMMGUI.markovExtractor ! TransitionsRequest(currentState)
      requestNextNote()
      initialized = true
      true
    } else {
      notify("No se ha podido inicializar el secuenciador, es necesario generar el modelo de Markov previamente.")
      false
    }
  }

  def startMelodyGeneration() = {
    if (!started) {
      initializeSequencer
      //sequencer.setTickPosition(0)
      sequencer.start()
      started = true
    } else {
      notify("La melodía ya se está generando")
    }
  }

  def stopMelodyGeneration() = {
    if (started) {
      sequencer.stop()
      started = false
    } else {
      notify("La melodía ya está parada")
    }
  }

  def updateSequencerTick(tick: Long) = {
    //notify("New Sequencer tick reached! " + tick)
    currentTick = tick
    val controlDurationIndex = (7 * currentCoords._1).round.toInt // Normalized to 8 possible durations
    val controlDuration = durations(controlDurationIndex)
    val controlNote = (23 * currentCoords._2).round.toInt
    kMMGUI.historyChart ! UpdateHistogram(controlNote, controlDuration, currentTick)
    if (currentTick >= currentNoteEndMidiEvent.getTick- 1) requestNextNote()
    else if (currentCoords._1 < lastNoteCoords._1) {
      val currentNoteElapsed = currentTick - (currentNoteEndMidiEvent.getTick - currentState.last._2)
      if (currentNoteElapsed > controlDuration) {
        cutCurrentNote()
        requestNextNote()
      }
    }
    //else updateFeedbackForce(controlNote, controlDuration)
  }

  /*def updateFeedbackForce(controlNote: Int, controlDuration: Int) = {
    val mostProbableTransition = currentStateTransitions.reduceLeft(mostProbable)
    notify("mostProbableTransition is: " + mostProbableTransition)
    /*var lowNearestNote = (-1, 0.0)
    var highNearestNote = (23, 0.0)
    var shortNearestDuration = (1, 0.0)
    var longNearestDuration = (16, 0.0)
    currentStateTransitions.foreach{
      case((note: Int, duration: Int), prob: Double) =>
        val noteDistance = note - controlNote
        val durationDistance = duration - controlDuration
        if (noteDistance < 0 && noteDistance >= lowNearestNote._1 - controlNote) lowNearestNote = (note, prob)
        if (noteDistance > 0 && noteDistance <= highNearestNote._1 - controlNote) highNearestNote = (note, prob)
        if (durationDistance < 0 && durationDistance >= shortNearestDuration._1 - controlDuration) shortNearestDuration = (duration, prob)
        if (durationDistance > 0 && durationDistance <= longNearestDuration._1) longNearestDuration = (duration, prob)
    }*/
    //TODO @ LAB - verify force feedback direction and scaling
    val scaling = 8
    val xVector: Float = (mostProbableTransition._1._2 - controlDuration).toFloat * scaling
    val yVector: Float = (mostProbableTransition._1._1 - controlNote).toFloat * scaling
    kMMGUI.deviceController ! UpdateFeedbackForce((xVector, yVector))
  }*/

  def mostProbable(t1: ((Int, Int), Double), t2: ((Int, Int), Double)): ((Int, Int), Double) = if (t1._2 > t2._2) t1 else t2

  def addNextNote(note: (Int, Int)) = {
    val currentNoteEndTick = currentNoteEndMidiEvent.getTick
    val endTick = currentNoteEndTick + note._2
    //Check if silence
    if (note._1 != -1) track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, note._1 + outNormalization, 127), currentNoteEndTick))
    else track.add(new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, 0, note._1 + outNormalization, 0), currentNoteEndTick))
    currentNoteEndMidiEvent = new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, note._1 + outNormalization, 127), endTick)
    track.add(currentNoteEndMidiEvent)
    currentState.append(note)
    kMMGUI.historyChart ! DrawNote(currentNoteEndTick, note._1, note._2)
    kMMGUI.markovExtractor ! TransitionsRequest(currentState)
    lastNoteCoords = currentCoords
  }

  def cutCurrentNote() = {
    track.remove(currentNoteEndMidiEvent)
    val currentNoteEndTick = currentTick + 1
    currentNoteEndMidiEvent = new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, 0, currentState.last._1 + outNormalization, 127), currentNoteEndTick)
    track.add(currentNoteEndMidiEvent)
    kMMGUI.historyChart ! DrawNoteCut(currentNoteEndTick, currentState.last._1)
  }

  def requestNextNote() = {
    if (currentStateTransitions.nonEmpty) {
      implicit val timeout = Timeout(80 milliseconds)
      val noteFuture = kMMGUI.kController ? CalcNoteOutputRequest(currentStateTransitions, currentCoords._1, currentCoords._2)
      val nextNote = Await.result(noteFuture, timeout.duration).asInstanceOf[(Int, Int)] // Warning, synchronous request!
      addNextNote(nextNote)
    }
    else {
      stopMelodyGeneration()
      notify("Error while trying to get Markov Transitions, please generate model first. Melody generation has been stopped.")
    }
  }

  def saveMidiTrack = {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH;mm;ss")
    val now = Calendar.getInstance.getTime
    val path = kMMGUI.notesDirField.text
    val outFile = new File(path +  "/kMarkovMelodyGeneratedSequence " + dateFormat.format(now) + ".mid")
    MidiSystem.write(sequence, 0, outFile)
    notify("Succesfully exported MIDI sequence in directory " +  path)
    true
  }

  def updateTempo(tempo: Int): Boolean = {
    if (tempo != currentTempo) {
      sequencer.setTempoInBPM(tempo)
      currentTempo = tempo
      notify("Tempo actualizado exitosamente a " + currentTempo + " BPMs")
      true
    } else false
  }

  def updateOutNormalization(norm: Byte): Boolean = {
    if (norm != outNormalization) {
      outNormalization = norm
      notify("Normalización actualizada exitosamente, octava inicial actual: " + norm/12)
      true
    } else false
  }

  def updateProgramChange(newProgramChange: Byte): Boolean = {
    if (newProgramChange != programChange) {
      programChange = newProgramChange
      track.add(new MidiEvent(new ShortMessage(ShortMessage.PROGRAM_CHANGE, 0, programChange, 0), currentTick + 1))
      notify("Instrumento actualizado exitosamente, MIDI program change actual: " + programChange)
      true
    } else false
  }

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def receive() = {
    case StartMelodyGenerationRequest => startMelodyGeneration()
    case StopMelodyGenerationRequest => stopMelodyGeneration()
    case NewSequencerTick(tick: Long) => updateSequencerTick(tick)
    case UpdateCoords(coords) => currentCoords = coords
    case InitializeState(state: FixedList[(Int, Int)]) => currentState = state
    case TransitionsList(list: List[((Int, Int), Double)]) =>
      kMMGUI.joystickChart ! TransitionsList(list)
      currentStateTransitions = list
      kMMGUI.deviceController ! MostProbableTransition(currentStateTransitions.reduceLeft(mostProbable))
      //notify("\nConductor received new transitions list!!!\n")
    case SaveMidiTrackRequest => saveMidiTrack
    case UpdateTempo(tempo: Int) => updateTempo(tempo)
    case UpdateOutputNormalization(norm: Byte) => updateOutNormalization(norm)
    case UpdateProgramChange(programChange: Byte) => updateProgramChange(programChange)
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
