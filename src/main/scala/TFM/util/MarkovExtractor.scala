package TFM.util

import java.io.File
import java.text.NumberFormat

import TFM.CommProtocol.{HMMExtractionRequest, TransitionsList, TransitionsRequest}
import TFM.kMarkovMelodyGenerator.kMMGUI
import TFM.util.SplitToTuple._
import akka.actor.Actor

import scala.collection.mutable.ArrayBuffer

/**
  * Created by diego on 6/05/16.
  */
class MarkovExtractor extends Actor{

  var markovChain = new MarkovChain[(Int, Int)]()
  val durations = List(1, 2, 3, 4, 6, 8, 12, 16)

  def extractMarkovChain(path: String) = {
    notify("Calculando Markov Chain con las notas y duración de los archivos txt en la carpeta: " + path)
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
    var prevStatus = (999, 999)
    var transitionsCount = 0
    normalizedNotesWithDurations.foreach { case (note: Int, duration: Int) =>
      if (prevStatus == (999, 999)) {
        prevStatus = (note, duration)
      } else {
        markovChain = markovChain.addTransition(prevStatus, (note, duration))
        transitionsCount += 1
        prevStatus = (note, duration)
      }
    }
    markovChain = markovChain.addTransition(normalizedNotesWithDurations.last, normalizedNotesWithDurations.head) // asegurarse de que siempre existen transiciones para cada estado
    notify(markovChain.states().toString())
    markovChain.states().foreach( state =>
      notify("transitions for " + state + ": " + markovChain.transitionsFor(state).toString())
    )
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
    case TransitionsRequest(state: (Int, Int)) => sender ! TransitionsList(markovChain.transitionsFor((normalizeNote(state._1), normalizeDuration(state._2))))
    case HMMExtractionRequest(path) => extractMarkovChain(path)
    case _ ⇒ println("FeaturesExtractor received unknown message")
  }

}
