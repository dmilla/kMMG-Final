package TFM.util

import java.io.File
import java.text.NumberFormat

import TFM.CommProtocol._
import TFM.kMarkovMelodyGenerator.kMMGUI
import TFM.util.SplitToTuple._
import akka.actor.Actor

import scala.collection.immutable.HashMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Created by diego on 6/05/16.
  */
class MarkovExtractor extends Actor{

  var order: Byte = 3
  var markovChain = new MarkovChain[(Int, Int)]()
  val durations = List(1, 2, 3, 4, 6, 8, 12, 16)

  def extractMarkovChain(path: String) = {
    notify("\nCalculando Cadena de Markov de orden " + order + " con las notas y duración de los archivos txt en la carpeta: " + path + "\n")
    markovChain = new MarkovChain[(Int, Int)]()
    val pathFile = new File(path)
    val notesWithDuration = ListBuffer.empty[(Int, Int)]
    var count = 0

    for(file <- pathFile.listFiles if file.getName endsWith ".txt"){
      try {
        val fileNotes = extractNotesWithDurationFromTxt(file)
        notify("Extraidas " + fileNotes.size + " notas de " + file.getName + " nota mínima: " + fileNotes.reduceLeft(lowestNote).toString + " - nota máxima: " + fileNotes.reduceLeft(highestNote).toString)
        notesWithDuration ++= fileNotes
        count += 1
      } catch {
        case e: Exception => notify("Excepción extrayendo notas del archivo " + file.getName + " en " + path + " : " + e);
      }
    }

    val notesWithDurationList = notesWithDuration.toList
    //finding best normalization
    val bestNormalization = findBestNormalization(notesWithDurationList)
    kMMGUI.conductor ! UpdateOutputNormalization(bestNormalization)
    kMMGUI.settings ! UpdateOutputNormalization(bestNormalization)

    val normalizedNotesWithDurations = normalizeNotesAndDurations(notesWithDurationList, bestNormalization)
    val prevStatus = new FixedList[(Int, Int)](order)
    var transitionsCount = 0
    normalizedNotesWithDurations.foreach { case (note: Int, duration: Int) =>
      if (!prevStatus.full) {
        prevStatus.append((note, duration))
      } else {
        val statusList = ListBuffer.empty[(Int, Int)]
        prevStatus.foreach(statusList += _)
        markovChain = markovChain.addTransition(statusList, (note, duration))
        transitionsCount += 1
        prevStatus.append((note, duration))
        //notify("prevStatus: " + prevStatus)
      }
    }

    // asegurarse de que siempre existen transiciones para cada estado
    (0 to order - 1).foreach{ case i: Int =>
      val statusList = ListBuffer.empty[(Int, Int)]
      prevStatus.foreach(statusList += _)
      markovChain = markovChain.addTransition(statusList, normalizedNotesWithDurations(i))
      prevStatus.append(normalizedNotesWithDurations(i))
    }

    kMMGUI.conductor ! InitializeState(prevStatus)
    kMMGUI.conductor ! TransitionsList(markovChain.transitionsFor(prevStatus.list))
    val statesCount = markovChain.states.size
    val controllableStatesCount = markovChain.controllableTransitionsCount()
    notify("\nModelo generado correctamente, número total de estados: " + statesCount)
    notify("Estados con más de una transición posible: " + ((controllableStatesCount.toDouble/statesCount) * 100).round + "% (" + controllableStatesCount + ")")
    /*markovChain.states().foreach( state =>
      notify("transitions for " + state + ": " + markovChain.transitionsFor(state).toString())
    )*/
    val normOctave = bestNormalization/12
    notify("\n¡Notas extraídas exitosamente de los " + count + " ficheros encontrados en " + path + "! Se ha generado un modelo de Markov con un total de " + NumberFormat.getIntegerInstance().format(transitionsCount) + " transiciones.\nLos datos han sido normalizados entre las octavas " + normOctave + " y " + (normOctave + 2))
  }

  def extractNotesWithDurationFromTxt(file: File) = {
    val source = scala.io.Source.fromFile(file)
    val fileNotesWithDurations = try source.mkString.replaceAll("[()]", "").split(" - ") finally source.close()
    fileNotesWithDurations.map(_.splitToTuple(",")).map{ case (a: String, b: String) => (a.toInt, b.toInt)}
  }

  def findBestNormalization(notesWithDuration: List[(Int, Int)]): Byte = {
    var silence = 0
    var octaveMinus2 = 0
    var octaveMinus1= 0
    var octave0 = 0
    var octave1 = 0
    var octave2 = 0
    var octave3 = 0
    var octave4 = 0
    var octave5 = 0
    var octave6 = 0
    var octave7 = 0
    var octave8 = 0
    notesWithDuration.foreach { case (note, duration) =>
      note match {
        case x if x < 0 => silence += 1
        case x if x < 12 => octaveMinus2 += 1
        case x if x < 24 => octaveMinus1 += 1
        case x if x < 36 => octave0 += 1
        case x if x < 48 => octave1 += 1
        case x if x < 60 => octave2 += 1
        case x if x < 72 => octave3 += 1
        case x if x < 84 => octave4 += 1
        case x if x < 96 => octave5 += 1
        case x if x < 108 => octave6 += 1
        case x if x < 120 => octave7 += 1
        case x if x < 128 => octave8 += 1
      }
    }
    val totalNotes: Double = notesWithDuration.length
    var currentBestNormalization: (Byte, Double) = (0, (octaveMinus2 + octaveMinus1 + octave0 + silence)/totalNotes)
    (1 to 8).foreach{ case i: Int =>
      i match {
        case 1 =>
          val totalCovered: Double = (octaveMinus1 + octave0 + octave1 + silence)/totalNotes
          if (totalCovered > currentBestNormalization._2) currentBestNormalization = (12, totalCovered)
        case 2 =>
          val totalCovered: Double = (octave0 + octave1 + octave2 + silence)/totalNotes
          if (totalCovered > currentBestNormalization._2) currentBestNormalization = (24, totalCovered)
        case 3 =>
          val totalCovered: Double = (octave1 + octave2 + octave3 + silence)/totalNotes
          if (totalCovered > currentBestNormalization._2) currentBestNormalization = (36, totalCovered)
        case 4 =>
          val totalCovered: Double = (octave2 + octave3 + octave4 + silence)/totalNotes
          if (totalCovered > currentBestNormalization._2) currentBestNormalization = (48, totalCovered)
        case 5 =>
          val totalCovered: Double = (octave3 + octave4 + octave5 + silence)/totalNotes
          if (totalCovered > currentBestNormalization._2) currentBestNormalization = (60, totalCovered)
        case 6 =>
          val totalCovered: Double = (octave4 + octave5 + octave6 + silence)/totalNotes
          if (totalCovered > currentBestNormalization._2) currentBestNormalization = (72, totalCovered)
        case 7 =>
          val totalCovered: Double = (octave5 + octave6 + octave7 + silence)/totalNotes
          if (totalCovered > currentBestNormalization._2) currentBestNormalization = (84, totalCovered)
        case 8 =>
          val totalCovered: Double = (octave6 + octave7 + octave8 + silence)/totalNotes
          if (totalCovered > currentBestNormalization._2) currentBestNormalization = (96, totalCovered)
      }
    }
    notify("\n¡Normalización calculada! Las notas entre " + currentBestNormalization._1 + " y " + (currentBestNormalization._1 + 35) + " representan el " + (currentBestNormalization._2 * 100).round + "% del total de las notas.\n")
    currentBestNormalization._1
  }

  def normalizeNotesAndDurations(notesWithDurations: List[(Int, Int)], bestNorm: Byte) = {
    val normalizedNotesWithDurations = ListBuffer.empty[(Int, Int)]
    val bestNormEnd = bestNorm + 35
    for ((note, duration) <- notesWithDurations) {
      val res = (normalizeNoteFlexible(note, bestNorm, bestNormEnd), normalizeDuration(duration))
      normalizedNotesWithDurations += res
    }
    normalizedNotesWithDurations.toList
  }

  //Normalize note to 3 octaves with best normalization found
  def normalizeNoteFlexible(note: Int, bestNormStart: Int, bestNormEnd: Int): Int = {
    var res: Int = 0
    note match {
      case -1 => res = note
      case x if x < bestNormStart => res = (note - (note / 12) * 12)
      case x if x >= bestNormStart && x <= bestNormEnd => res = note - bestNormStart
      case x if x > bestNormEnd => res = ((note - (note / 12) * 12) + 24)
    }
    res
  }

  def normalizeDuration(duration: Int): Int = {
    var res: Int = 0
    duration match {
      case x if x < 2   => res = 1
      case x if x == 2  => res = duration
      case x if x == 3  => res = duration
      case x if x == 4  => res = duration
      case x if x < 7   => res = 6
      case x if x == 8  => res = duration
      case x if x < 13  => res = 12
      case x if x > 12  => res = 16
    }
    res
  }

  //Normalize note to 2 octaves
  def normalizeNote(note: Int): Int = {
    var res: Int = 0
    note match {
      case x if x < 24  => res = note
      case x if x < 48  => res = note - 24
      case x if x < 72  => res = note - 48
      case x if x < 96  => res = note - 72
      case x if x < 120 => res = note - 96
      case x if x > 119 => res = note - 120
    }
    res
  }

  def lowestNote(t1: (Int, Int), t2: (Int, Int)): (Int, Int) = if (t1._1 < t2._1 && t1._1 != -1) t1 else t2

  def highestNote(t1: (Int, Int), t2: (Int, Int)): (Int, Int) = if (t1._1 > t2._1) t1 else t2

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def receive = {
    case TransitionsRequest(state: FixedList[(Int, Int)]) => sender ! TransitionsList(markovChain.transitionsFor(state.list))
    case HMMExtractionRequest(path) => extractMarkovChain(path)
    case UpdateMarkovOrder(newOrder: Byte) => order = newOrder
    case _ ⇒ println("FeaturesExtractor received unknown message")
  }

}
