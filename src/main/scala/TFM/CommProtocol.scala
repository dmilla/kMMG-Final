package TFM

import java.io.File

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
  case class UpdateCoords(coords: (Double, Double))
  case class SetVisible()
  case class CheckSequencerTick()
  case class NewSequencerTick(tick: Long)

}
