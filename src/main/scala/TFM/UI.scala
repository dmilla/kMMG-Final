package TFM

/**
  * Created by diego on 05/05/16.
  */
import java.io.File
import java.text.NumberFormat

import TFM.CommProtocol._
import akka.actor.{ActorSystem, Props}

import scala.swing._


class UI extends MainFrame {

  title = "Controlled Markov Melody Generator - Diego Milla - TFM - Máster SSII - USAL"
  preferredSize = new Dimension(1500, 1000)

  val controlSystem = ActorSystem("controlSystem")
  val watcher = controlSystem.actorOf(Props(classOf[DeviceWatcher]))
  val deviceController = controlSystem.actorOf(Props(classOf[DeviceController]))
  val markovExtractor = controlSystem.actorOf(Props(classOf[MarkovExtractor]))
  //val midiSender = controlSystem.actorOf(Props[MidiSender])
  val controller = controlSystem.actorOf(Props(classOf[KController]))
  val conductor = controlSystem.actorOf(Props[Conductor])
  val joystickChart = controlSystem.actorOf(Props[JoystickChart])

  val textFieldSize = new Dimension(360, 25)
  val labelSize = new Dimension(300, 25)
  val numberFieldSize = new Dimension(60, 25)

  val outputField = new TextArea { rows = 26; lineWrap = true; wordWrap = true; editable = false }
  val defaultPathFile = new File(System.getProperty("user.home") + "/MidiWebMiner/notes with duration/Piano") // TODO inicializar directorio en carpeta general
  //defaultPathFile.mkdirs
  val notesDirChooser = new FileChooser(defaultPathFile)
  val notesDirField = new TextField( defaultPathFile.getAbsolutePath )
  notesDirChooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
  notesDirChooser.title = "Selecciona el directorio con las secuencias de notas objetivo MIDIs"
  notesDirField.peer.setMaximumSize(textFieldSize)
  notesDirField.editable = false

  //TODO - USE FormattedTextFields to avoid bad entries in number fields
  val integerFieldFormatter = NumberFormat.getIntegerInstance()

  contents = new BoxPanel(Orientation.Vertical) {

    contents += new BoxPanel(Orientation.Horizontal) {
      val label = new Label("Directorio de las secuencias de notas")
      label.peer.setMaximumSize(labelSize)
      label.horizontalAlignment = Alignment.Left
      contents += label
      contents += Swing.HStrut(5)
      contents += notesDirField
      contents += Button("Seleccionar") {
        val res = notesDirChooser.showOpenDialog(this)
        if (res == FileChooser.Result.Approve) {
          notesDirField.text = notesDirChooser.selectedFile.getPath
        } else None
      }
      contents += Swing.HStrut(5)
      contents += Button("Generar Modelo Markov") {
        markovExtractor ! HMMExtractionRequest(notesDirField.text)
      }
      contents += Swing.HStrut(260)
      contents += Button("Ver Gráfico Joystick") {
        joystickChart ! SetVisible
      }
    }
    contents += Button("Generar Melodía") {
      conductor ! StartMelodyGenerationRequest
    }
    contents += Swing.VStrut(10)

    contents += new Label("Información")
    contents += Swing.VStrut(3)
    contents += new ScrollPane(outputField)
    contents += Swing.VStrut(10)
    for (e <- contents)
      e.xLayoutAlignment = 0.0
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

 /* def updateState(state: Int) = {
    lastNoteField.text = state.toString
    markovExtractor ! UpdateMarkovProbsRequest(state)
  }
*/
  def addOutput(out: String): Unit = {
    outputField.append(out + "\n")
    outputField.peer.setCaretPosition(outputField.peer.getDocument.getLength)
  }


}


