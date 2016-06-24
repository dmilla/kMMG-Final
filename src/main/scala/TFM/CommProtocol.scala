package TFM

import java.io.File

import akka.actor.ActorRef
import org.jfree.chart.ChartPanel

/**
  * Created by diego on 30/03/16.
  * */

object CommProtocol {

  case class CrawlRequest(url: String, followIf: String, depth: Int, downloadsDirectory: String)
  case class NotesExtractionRequest(midiFile: File)
  case class FolderNotesExtractionRequest(path: String)
  case class FeaturesExtractionRequest(path: String)
  case class HMMExtractionRequest(path: String)
  case class SendMidiNoteRequest(note: (Int, Int))
  case class CalcNoteOutputRequest(markovProbabilities: List[((Int, Int), Double)], xPosition: Double, yPosition: Double)
  case class TransitionsRequest(state: (Int, Int))
  case class UpdateStatus(state: (Int, Int))
  case class ConnectToDeviceRequest(port: String)
  case class NotifyNoteFinished()
  case class StartMelodyGenerationRequest()
  case class StopMelodyGenerationRequest()
  case class UpdateCoords(coords: (Double, Double))
  case class SetVisible()
  case class CheckSequencerTick()
  case class NewSequencerTick(tick: Long)
  case class TransitionsList(list: List[((Int, Int), Double)])
  case class UpdateFeedbackForce(forceVector: (Float, Float))
  case class UpdateHistogram(controlNote: Int, controlDuration: Int, tick: Long)
  case class DrawNote(tick: Long, note: Int, duration: Int)
  case class DrawNoteCut(tick: Long, note: Int)
  case class EndManualControlRequest()
  case class SaveMidiTrackRequest()

}
