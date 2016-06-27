package TFM.kMarkovMelodyGenerator

import java.text.DecimalFormat

import TFM.CommProtocol._
import akka.actor.Actor

/**
  * Created by diego on 9/05/16.
  */
class KController extends Actor{

  var k: Double = 0.8
  var maxNoteDistanceToControl = 6
  var maxDurationDistanceToControl = 3

  val formatter = new DecimalFormat("#.##")
  val durations = List(1, 2, 3, 4, 6, 8, 12, 16)

  //TODO - add max distance to control GUI setting
  def calcNoteOutput(markovProbabilites: List[((Int, Int), Double)], xPosition: Double, yPosition: Double) = {
    val controlNote = (24 * yPosition).round.toInt - 1 // Normalized to two octaves and -1 for silence
    val controlDurationIndex = (7 * xPosition).round.toInt // Normalized to 8 possible durations
    val controlDuration = durations(controlDurationIndex)
    //notify("Initial Markov probabilites: " + markovProbabilites)
    notify("Control Note is: " + controlNote + " -  Control Duration is: " + controlDuration)
    val controlProbabilities = calcControlProbabilities(markovProbabilites, controlNote, controlDuration).toMap
    //notify("Final output probabilities: " + controlProbabilities)
    val out = sample[(Int, Int)](controlProbabilities)
    notify("Nota de salida calculada: " + out + " - Se reproducirá una nota normalizada en función de los ajustes")
    out
  }

  def calcControlProbabilities(markovProbabilites: List[((Int, Int), Double)], controlNote: Int, controlDuration: Int) = {
    val probs = scala.collection.mutable.Map[(Int, Int), Double]()
    /*var lowNearestNote = -1
    var highNearestNote = 23
    var shortNearestDuration = 1
    var longNearestDuration = 16
    markovProbabilites.foreach{
      case((note: Int, duration: Int), prob: Double) =>
        val noteDistance = note - controlNote
        val durationDistance = duration - controlDuration
        if (noteDistance < 0 && noteDistance >= lowNearestNote - controlNote) lowNearestNote = note
        if (noteDistance > 0 && noteDistance <= highNearestNote - controlNote) highNearestNote = note
        if (durationDistance < 0 && durationDistance >= shortNearestDuration - controlDuration) shortNearestDuration = duration
        if (durationDistance > 0 && durationDistance <= longNearestDuration) longNearestDuration = duration
    }
    markovProbabilites.foreach{
      case((note: Int, duration: Int), prob: Double) =>
        var outProb: Double = prob * k
        if (note == lowNearestNote) outProb += (1.0 - k) * 0.25
        if (note == highNearestNote) outProb += (1.0 - k) * 0.25
        if (duration == shortNearestDuration) outProb += (1.0 - k) * 0.25
        if (duration == longNearestDuration) outProb += (1.0 - k) * 0.25
        probs += ((note, duration) -> outProb)
    }*/
    // Old way to calc probs with distance
    markovProbabilites.foreach{
      case((note, duration), prob) =>
        val noteDistance: Int = math.abs(controlNote - note)
        val durationDistance: Int = math.abs(durations.indexOf(controlDuration) - durations.indexOf(duration))
        if (noteDistance <= maxNoteDistanceToControl && durationDistance <= maxDurationDistanceToControl) probs += ((note, duration) -> calcNoteAndDurationProbability(prob, note, noteDistance, duration, durationDistance))
    }
    if (probs.isEmpty) {
      markovProbabilites.foreach{
        case((note, duration), prob) =>
          probs += ((note, duration) -> prob)
      }
    }
    probs
  }

  def calcNoteAndDurationProbability(markovProbability: Double, note: Int, noteDistance: Int, duration: Int, durationDistance: Int): Double = {
    var outProb: Double = (1.0 - k) * markovProbability
    if (noteDistance == 0 && durationDistance == 0) {
      val increase: Double = k * 0.4
      notify(increase * 100 + "% prob increase for note " + note + " with duration " + duration)
      outProb += increase
    } else if ((noteDistance == 0 && durationDistance == 1) || (durationDistance == 0 && noteDistance == 1)) {
      val increase: Double = k * 0.1
      notify(increase * 100 + "% prob increase for note " + note + " with duration " + duration)
      outProb += increase
    } else if (noteDistance <= 1 && durationDistance <= 1) {
      val increase: Double = k * 0.05
      notify(increase * 100 + "% prob increase for note " + note + " with duration " + duration)
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

  def updateK(newK: Double) = {
    if (newK != k && newK <= 1 && newK >= 0) {
      k = newK
      notify("Parámetro K (peso del control) ajustado correctamente a " + k)
      true
    } else false
  }

  def updateMaxDurationDistance(distance: Byte): Boolean = {
    if (distance != maxDurationDistanceToControl) {
      maxDurationDistanceToControl = distance
      notify("Distancia máxima de la duración de salida respecto al control ajustada correctamente a " + maxDurationDistanceToControl)
      true
    } else false
  }

  def updateMaxNoteDistance(distance: Byte): Boolean = {
    if (distance != maxNoteDistanceToControl) {
      maxNoteDistanceToControl = distance
      notify("Distancia máxima de la nota de salida respecto al control ajustada correctamente a " + maxNoteDistanceToControl)
      true
    } else false
  }

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def receive: Receive = {
    case CalcNoteOutputRequest(markovProbabilities, xPosition, yPosition) => sender ! calcNoteOutput(markovProbabilities, xPosition, yPosition)
    case UpdateMaxDurationDistanceToControl(distance: Byte) => updateMaxDurationDistance(distance)
    case UpdateMaxNoteDistanceToControl(distance: Byte) => updateMaxNoteDistance(distance)
    case UpdateK(k: Double) => updateK(k)
    case _ ⇒ println("kController received unknown message")
  }
}
