package TFM.kMarkovMelodyGenerator

/**
  * Created by diego on 05/05/16.
  */
import java.io.File
import java.text.NumberFormat

import TFM.CommProtocol._
import TFM.MidiWebMiner.WebMinerUI
import TFM.device.{DeviceController, DeviceWatcher}
import TFM.util.MarkovExtractor
import TFM.kMarkovMelodyGenerator.charts.{HistoryChart, JoystickChart}
import akka.actor.{ActorSystem, Props}

import scala.swing._

// TODO - improve web miner + integrate it (directories etc)
// TODO - add sequencer like UI
// TODO - only show start/stop melody buttons if model generated previously => BEST POSSIBLE UX!

class UI extends MainFrame {

  title = "Kontrolled Markov Melody Generator - Diego Milla - TFM - Máster SSII - USAL"
  preferredSize = new Dimension(1200, 680)

  val midiWebMiner = new WebMinerUI

  val controlSystem = ActorSystem("controlSystem")
  val watcher = controlSystem.actorOf(Props(classOf[DeviceWatcher]))
  val deviceController = controlSystem.actorOf(Props(classOf[DeviceController]))
  val markovExtractor = controlSystem.actorOf(Props(classOf[MarkovExtractor]))
  val kController = controlSystem.actorOf(Props(classOf[KController]))
  val conductor = controlSystem.actorOf(Props[Conductor])
  val joystickChart = controlSystem.actorOf(Props[JoystickChart])
  val historyChart = controlSystem.actorOf(Props[HistoryChart])
  val settings = controlSystem.actorOf(Props[SettingsFrame])

  val textFieldSize = new Dimension(600, 25)
  val labelSize = new Dimension(300, 25)
  val numberFieldSize = new Dimension(60, 25)

  val outputField = new TextArea { rows = 26; lineWrap = true; wordWrap = true; editable = false }
  val defaultPathFile = new File(System.getProperty("user.home") + "/MidiWebMiner/new/beethoven/notes with duration/Piano") // TODO inicializar directorio en carpeta general
  //defaultPathFile.mkdirs
  val notesDirChooser = new FileChooser(defaultPathFile)
  val notesDirField = new TextField( defaultPathFile.getAbsolutePath )
  notesDirChooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
  notesDirChooser.title = "Selecciona el directorio con las secuencias de notas objetivo MIDIs"
  notesDirField.peer.setMaximumSize(textFieldSize)
  notesDirField.editable = false

  val orderField = new ComboBox(1 to 8)
  orderField.peer.setPreferredSize(numberFieldSize)
  orderField.peer.setMaximumSize(numberFieldSize)

  val startButton = Button("Generar Melodía") {
    conductor ! StartMelodyGenerationRequest
  }
  val stopButton = Button("Parar Melodía") {
    conductor ! StopMelodyGenerationRequest
  }
  val exportButton = Button("Exportar en formato MIDI") {
    conductor ! SaveMidiTrackRequest
  }
  val chartsButton = Button("Ver Gráficos") {
    historyChart ! SetVisible
    joystickChart ! SetVisible
  }

  val xForceField = new TextField { text = "10" }
  xForceField.peer.setMaximumSize(numberFieldSize)
  val yForceField = new TextField { text = "10" }
  yForceField.peer.setMaximumSize(numberFieldSize)

  //TODO - USE FormattedTextFields to avoid bad entries in number fields
  val integerFieldFormatter = NumberFormat.getIntegerInstance()

  contents = new BoxPanel(Orientation.Vertical) {

    val label = new Label("Directorio de las secuencias de notas")
    label.peer.setMaximumSize(labelSize)
    label.horizontalAlignment = Alignment.Left
    contents += label

    contents += Swing.VStrut(3)

    contents += new BorderPanel {
      val dataPanel = new BoxPanel(Orientation.Horizontal) {
        contents += Swing.HStrut(5)
        contents += notesDirField
        contents += Button("Seleccionar") {
          val res = notesDirChooser.showOpenDialog(this)
          if (res == FileChooser.Result.Approve) {
            notesDirField.text = notesDirChooser.selectedFile.getPath
          } else None
        }
      }
      add(dataPanel, BorderPanel.Position.West)
      val minerPanel = new BoxPanel(Orientation.Horizontal) {
        contents += Button("Midi Web Miner") {
          midiWebMiner.open
        }
      }
      add(minerPanel, BorderPanel.Position.East)
    }

    contents += Swing.VStrut(10)

    contents += new Label("Orden del Modelo de Markov")
    contents += Swing.VStrut(3)

    contents += new BorderPanel {
      val markovPanel = new BoxPanel(Orientation.Horizontal) {
        contents += orderField
        contents += Swing.HStrut(5)
        contents += Button("Generar Modelo") {
          markovExtractor ! UpdateMarkovOrder(orderField.peer.getSelectedItem.toString.toByte)
          markovExtractor ! HMMExtractionRequest(notesDirField.text)
        }
      }
      add(markovPanel, BorderPanel.Position.West)
      val chartsPanel = new BoxPanel(Orientation.Horizontal) {
        contents += chartsButton
      }
      add(chartsPanel, BorderPanel.Position.East)
    }




    contents += Swing.VStrut(10)

    /*contents += new BoxPanel(Orientation.Horizontal) {
      val label = new Label("Vector de fuerza - X: ")
      contents += label
      contents += Swing.HStrut(5)
      contents += xForceField
      contents += Swing.HStrut(5)
      contents += new Label(" - Y: ")
      contents += Swing.HStrut(5)
      contents += yForceField
      contents += Swing.HStrut(5)
      contents += Button("Probar") {
        deviceController ! UpdateFeedbackForce((xForceField.text.toFloat, yForceField.text.toFloat))
      }
    }*/

    contents += new Label("Reproducción")
    contents += Swing.VStrut(3)
    contents += new BorderPanel {
      val ssPanel = new BoxPanel(Orientation.Horizontal) {
        contents += startButton
        contents += Swing.HStrut(5)
        contents += stopButton
      }
      val bPanel = new BoxPanel(Orientation.Horizontal){
        contents += Button("Ajustes") {
          settings ! SetVisible
        }
        contents += Swing.HStrut(5)
        contents += exportButton
      }
      add(ssPanel, BorderPanel.Position.West)
      add(bPanel, BorderPanel.Position.East)
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

  def addOutput(out: String): Unit = {
    outputField.append(out + "\n")
    outputField.peer.setCaretPosition(outputField.peer.getDocument.getLength)
  }

}



