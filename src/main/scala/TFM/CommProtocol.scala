package TFM

import java.io.File

import TFM.util.FixedList

/**
  * Created by diego on 30/03/16.
  * */

object CommProtocol {

  // Markov-related Messages
  case class HMMExtractionRequest(path: String)
  case class UpdateMarkovOrder(order: Byte)
  case class TransitionsRequest(status: FixedList[(Int, Int)])
  case class TransitionsList(list: List[((Int, Int), Double)])
  case class InitializeState(state: FixedList[(Int, Int)])
  case class MostProbableTransition(transition: ((Int, Int), Double))

  // Control Messages
  case class UpdateCoords(coords: (Double, Double))
  case class CalcNoteOutputRequest(markovProbabilities: List[((Int, Int), Double)], xPosition: Double, yPosition: Double)
  case class UpdateK(k: Double)
  case class UpdateMaxNoteDistanceToControl(distance: Byte)
  case class UpdateMaxDurationDistanceToControl(distance: Byte)
  case class UpdateTempo(tempo: Int)
  case class UpdateOutputNormalization(norm: Byte)
  case class UpdateProgramChange(programChange: Byte)

  // Playback Messages
  case class StartMelodyGenerationRequest()
  case class StopMelodyGenerationRequest()
  case class CheckSequencerTick()
  case class NewSequencerTick(tick: Long)
  case class SaveMidiTrackRequest()

  // UI Messages
  case class SetVisible()
  case class UpdateHistogram(controlNote: Int, controlDuration: Int, tick: Long)
  case class DrawNote(tick: Long, note: Int, duration: Int)
  case class DrawNoteCut(tick: Long, note: Int)

  // Device Messages
  case class ConnectToDeviceRequest(port: String)
  case class EndManualControlRequest()
  case class UpdateFeedbackForce(forceVector: (Float, Float))

  // Midi Web Miner Messages
  case class CrawlRequest(url: String, followIf: String, depth: Int, downloadsDirectory: String)
  case class NotesExtractionRequest(midiFile: File)
  case class FolderNotesExtractionRequest(path: String)
  case class FeaturesExtractionRequest(path: String)

}
