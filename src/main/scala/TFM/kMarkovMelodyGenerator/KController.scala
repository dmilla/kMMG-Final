package TFM.kMarkovMelodyGenerator

import java.text.DecimalFormat

import TFM.CommProtocol.CalcNoteOutputRequest
import akka.actor.Actor

/**
  * Created by diego on 9/05/16.
  */
class KController extends Actor{

  var affectedNotes = 1
  var k: Double = 0.5

  val formatter = new DecimalFormat("#.##")
  val durations = List(1, 2, 3, 4, 6, 8, 12, 16)

  //TODO - add max distance to control
  //TODO - adapt to -1 = silence note!!
  def calcNoteOutput(markovProbabilites: List[((Int, Int), Double)], xPosition: Double, yPosition: Double) = {
    val controlNote = ((24 - 1) * yPosition).round.toInt // Normalized to two octaves
    val controlDurationIndex = (7 * xPosition).round.toInt // Normalized to 8 possible durations
    val controlDuration = durations(controlDurationIndex)
    //notify("Initial Markov probabilites: " + markovProbabilites)
    notify("Control Note is: " + controlNote + " -  Control Duration is: " + controlDuration)
    val controlProbabilities = calcControlProbabilities(markovProbabilites, controlNote, controlDuration).toMap
    //notify("Final output probabilities: " + controlProbabilities)
    val out = sample[(Int, Int)](controlProbabilities)
    //kMMGUI.conductor ! UpdateStatus(out)
    notify("Nota de salida calculada: " + out + " - Se reproducirá una nota normalizada 36 notas por encima")
    //kMMGUI.midiSender ! SendMidiNoteRequest((out._1 + 36, out._2))
    //ui.midiSender ! SendMidiNoteRequest((out + 48,  1)) //TODO normalize output GUI setting
    //kMMGUI.updateState(out)
    out
  }

  def calcControlProbabilities(markovProbabilites: List[((Int, Int), Double)], controlNote: Int, controlDuration: Int) = {
    val probs = scala.collection.mutable.Map[(Int, Int), Double]()
    markovProbabilites.foreach{
      case((note, duration), prob) =>
        probs += ((note, duration) -> calcNoteAndDurationProbability(prob, note, controlNote, duration, controlDuration))
    }
    probs
  }

  // TODO - iterate over all notes!! relative instead of absolute distance (nearests notes, even if faaaar away :D )
  def calcNoteAndDurationProbability(markovProbability: Double, note: Int, controlNote: Int, duration: Int, controlDuration: Int): Double = {
    val noteDistance: Int = math.abs(controlNote - note)
    val durationDistance: Int = math.abs(controlDuration - duration)
    var outProb: Double = k * markovProbability
    if (noteDistance == 0 && durationDistance == 0) {
      val increase: Double = (1.0 - k) * 0.4
      notify(increase * 100 + "% prob increase for note " + note + " with duration " + duration)
      outProb += increase
    } else if ((noteDistance == 0 && durationDistance == 1) || (durationDistance == 0 && noteDistance == 1)) {
      val increase: Double = (1.0 - k) * 0.1
      notify(increase * 100 + "% prob increase for note " + note + " with duration " + duration)
      outProb += increase
    } else if (noteDistance <= 1 && durationDistance <= 1) {
      val increase: Double = (1.0 - k) * 0.05
      notify(increase * 100 + "% prob increase for note " + note + " with duration " + duration)
      outProb += increase
    }
    outProb
  }

  def calcNoteProbability(markovProbability: Double, note: Int, controlNote: Int): Double = {
    val distance: Int = math.abs(controlNote - note)
    var outProb: Double = k * markovProbability
    if (distance <= affectedNotes) {
      val increase: Double = (1.0 - k) * (1.0/((affectedNotes * 2.0) + 1.0))
      notify(increase * 100 + "% prob increase for note " + note)
      outProb += increase
    }
    outProb
  }

  final def sample[A](dist: Map[A, Double]): A = {
    val p = scala.util.Random.nextDouble * dist.values.sum
    val it = dist.iterator
    var accum = 0.0
    while (it.hasNext) {
      val (item, itemProb) = it.next
      accum += itemProb
      if (accum >= p)
        return item  // return so that we don't have to search through the whole distribution
    }
    sys.error(f"this should never happen")  // needed so it will compile
  }

  def updateAffectedNotes(newValue: Int) ={
    affectedNotes = newValue
  }

  def updateK(newValue: Double) ={
    k = newValue
  }

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def receive: Receive = {
    case CalcNoteOutputRequest(markovProbabilities, xPosition, yPosition) => sender ! calcNoteOutput(markovProbabilities, xPosition, yPosition)
    case _ ⇒ println("kController received unknown message")
  }
}
