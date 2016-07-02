package TFM.util

import java.io.File
import java.text.NumberFormat

import TFM.CommProtocol._
import TFM.kMarkovMelodyGenerator.kMMGUI
import TFM.util.SplitToTuple._
import akka.actor.Actor

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
    val notesWithDuration = ArrayBuffer.empty[(Int, Int)]
    var count = 0
    for(file <- pathFile.listFiles if file.getName endsWith ".txt"){
      try {
        val fileNotes = extractNotesWithDurationFromTxt(file)
        notify("Extraidas " + fileNotes.size + " notas de " + file.getName)
        notesWithDuration ++= fileNotes
        count += 1
      } catch {
        case e: Exception => notify("Excepción extrayendo notas del archivo " + file.getName + " en " + path + " : " + e);
      }
    }
    val normalizedNotesWithDurations = normalizeNotesAndDurations(notesWithDuration)
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
    notify("\n¡Notas extraídas exitosamente de los " + count + " ficheros encontrados en " + path + "! Se ha generado un modelo de Markov con un total de " + NumberFormat.getIntegerInstance().format(transitionsCount) + " transiciones")
  }

  def extractNotesWithDurationFromTxt(file: File) = {
    val source = scala.io.Source.fromFile(file)
    val fileNotesWithDurations = try source.mkString.replaceAll("[()]", "").split(" - ") finally source.close()
    fileNotesWithDurations.map(_.splitToTuple(",")).map{ case (a: String, b: String) => (a.toInt, b.toInt)}
  }

  def normalizeNotesAndDurations(notesWithDurations: ArrayBuffer[(Int, Int)]) = {
    val normalizedNotesWithDurations = ArrayBuffer.empty[(Int, Int)]
    for ((note, duration) <- notesWithDurations) {
      val res = (normalizeNote(note), normalizeDuration(duration))
      normalizedNotesWithDurations += res
    }
    normalizedNotesWithDurations.toVector
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

  def notify(msg: String) = kMMGUI.addOutput(msg)

  def receive = {
    case TransitionsRequest(state: FixedList[(Int, Int)]) => sender ! TransitionsList(markovChain.transitionsFor(state.list))
    case HMMExtractionRequest(path) => extractMarkovChain(path)
    case UpdateMarkovOrder(newOrder: Byte) => order = newOrder
    case _ ⇒ println("FeaturesExtractor received unknown message")
  }

}
