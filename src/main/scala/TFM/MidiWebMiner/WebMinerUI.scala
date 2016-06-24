package TFM.MidiWebMiner

/**
  * Created by diego on 25/03/16.
  */
import java.awt.BorderLayout
import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import javax.swing.JPanel

import TFM.CommProtocol._
import akka.actor.{ActorSystem, Props}

import scala.swing._

// TODO - extract features after process and show summary!
class WebMinerUI extends Frame {

  title = "MIDI Web Miner"
  preferredSize = new Dimension(1250, 750)
  val parentPanel = new JPanel(new BorderLayout())

  val actorSystem = ActorSystem("MidiMiningSystem")
  val crawler = actorSystem.actorOf(Props[WebCrawler])
  val notesExtractor = actorSystem.actorOf(Props[NotesExtractor])
  val featuresExtractor = actorSystem.actorOf(Props[FeaturesExtractor])

  val textFieldSize = new Dimension(300, 25)
  val labelSize = new Dimension(250, 25)
  val depthSize = new Dimension(60, 25)

  val nameField = new TextField { text = "http://www.download-midi.com/" }
  val followField = new TextField { text = "midi"}
  val depthField = new TextField { text = "2" }
  nameField.peer.setMaximumSize(textFieldSize)
  followField.peer.setMaximumSize(textFieldSize)
  depthField.peer.setMaximumSize(depthSize)

  val crawlerOutputField = new TextArea { rows = 12; lineWrap = true; wordWrap = true; editable = false }
  val extractorOutputField = new TextArea { rows = 12; lineWrap = true; wordWrap = true; editable = false }
  val defaultPathFile = new File(System.getProperty("user.home") + "/MidiWebMiner")
  defaultPathFile.mkdirs
  val crawlDirChooser = new FileChooser(defaultPathFile)
  val midiExtractDirChooser = new FileChooser(defaultPathFile)
  val featuresExtractDirChooser = new FileChooser(defaultPathFile)
  val crawlerDirField = new TextField( defaultPathFile.getAbsolutePath )
  crawlDirChooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
  crawlDirChooser.title = "Selecciona el directorio en el que se descargarán los MIDIs"
  crawlerDirField.peer.setMaximumSize(textFieldSize)
  crawlerDirField.editable = false
  val midiExtractorDirField = new TextField( defaultPathFile.getAbsolutePath )
  midiExtractDirChooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
  midiExtractDirChooser.title = "Selecciona el directorio en el que se encuentran los MIDIs para la extraccion"
  midiExtractorDirField.peer.setMaximumSize(textFieldSize)
  midiExtractorDirField.editable = false
  val featuresExtractorDirField = new TextField( defaultPathFile.getAbsolutePath + "/notes/Piano" )
  featuresExtractDirChooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
  featuresExtractDirChooser.title = "Selecciona el directorio de los archivos txt con las notas para la extraccion"
  featuresExtractorDirField.peer.setMaximumSize(textFieldSize)
  featuresExtractorDirField.editable = false
  val dateFormat = new SimpleDateFormat("d-M-y HH:mm:ss")

  contents = new BoxPanel(Orientation.Vertical) {
    contents += new BoxPanel(Orientation.Horizontal) {
      val label = new Label("Web Objetivo")
      label.peer.setMaximumSize(labelSize)
      label.horizontalAlignment = Alignment.Left
      contents += label
      contents += Swing.HStrut(5)
      contents += nameField
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      val label = new Label("Seguir enlaces con la palabra")
      label.peer.setMaximumSize(labelSize)
      label.horizontalAlignment = Alignment.Left
      contents += label
      contents += Swing.HStrut(5)
      contents += followField
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      val label = new Label("Profundidad Máxima")
      label.peer.setMaximumSize(labelSize)
      label.horizontalAlignment = Alignment.Left
      contents += label
      contents += Swing.HStrut(5)
      contents += depthField
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      val label = new Label("Directorio de Salida")
      label.peer.setMaximumSize(labelSize)
      label.horizontalAlignment = Alignment.Left
      contents += label
      contents += Swing.HStrut(5)
      contents += crawlerDirField
      contents += Button("Seleccionar") {
        val res = crawlDirChooser.showOpenDialog(this)
        if (res == FileChooser.Result.Approve) {
          crawlerDirField.text = crawlDirChooser.selectedFile.getPath
        } else None
      }
    }
    contents += Swing.VStrut(10)
    contents += Button("Empezar el proceso de Minería Web") {
      crawlerOutputField.peer.setText(dateFormat.format(Calendar.getInstance().getTime())+"\n")
      extractorOutputField.peer.setText(dateFormat.format(Calendar.getInstance().getTime())+"\n")
      crawler ! CrawlRequest(nameField.text, followField.text, depthField.text.toInt, crawlerDirField.text)
    }
    contents += Swing.VStrut(10)
    contents += new Label("Información del crawler")
    contents += Swing.VStrut(3)
    contents += new ScrollPane(crawlerOutputField)
    contents += Swing.VStrut(10)
    contents += new BoxPanel(Orientation.Horizontal) {
      val label = new Label("Extraer notas de MIDIs")
      label.peer.setMaximumSize(labelSize)
      label.horizontalAlignment = Alignment.Left
      contents += label
      contents += Swing.HStrut(5)
      contents += midiExtractorDirField
      contents += Button("Seleccionar") {
        val res = midiExtractDirChooser.showOpenDialog(this)
        if (res == FileChooser.Result.Approve) {
          midiExtractorDirField.text = midiExtractDirChooser.selectedFile.getPath
        } else None
      }
      contents += Swing.HStrut(5)
      contents += Button("Extraer Notas") { notesExtractor ! FolderNotesExtractionRequest(midiExtractorDirField.text) }
    }
    contents += Swing.VStrut(5)
    contents += new BoxPanel(Orientation.Horizontal) {
      val label = new Label("Extraer características de notas")
      label.peer.setMaximumSize(labelSize)
      label.horizontalAlignment = Alignment.Left
      contents += label
      contents += Swing.HStrut(5)
      contents += featuresExtractorDirField
      contents += Button("Seleccionar") {
        val res = featuresExtractDirChooser.showOpenDialog(this)
        if (res == FileChooser.Result.Approve) {
          featuresExtractorDirField.text = featuresExtractDirChooser.selectedFile.getPath
        } else None
      }
      contents += Swing.HStrut(5)
      contents += Button("Extraer Características") { featuresExtractor ! FeaturesExtractionRequest(featuresExtractorDirField.text) }
    }
    contents += Swing.VStrut(10)
    contents += new Label("Información de los extractores")
    contents += Swing.VStrut(3)
    contents += new ScrollPane(extractorOutputField)
    for (e <- contents)
      e.xLayoutAlignment = 0.0
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  def addCrawlerOutput(out: String): Unit = {
    crawlerOutputField.append(out + "\n")
  }

  def addExtractorOutput(out: String): Unit = {
    extractorOutputField.append(out + "\n")
  }

}



