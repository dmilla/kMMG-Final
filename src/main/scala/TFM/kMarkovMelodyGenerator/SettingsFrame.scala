package TFM.kMarkovMelodyGenerator

import java.awt.{BorderLayout, Dimension}
import javax.swing.{JFrame, JPanel}

import TFM.CommProtocol._
import akka.actor.Actor

import scala.swing._

/**
  * Created by diego on 27/06/16.
  */

//TODO - estrechar campos de entrada
class SettingsFrame extends JFrame  with Actor{

  setTitle("Ajustes")
  setSize(new Dimension(900, 300))
  setLocation(600, 300)

  val parentPanel = new JPanel(new BorderLayout())
  val textFieldSize = new Dimension(360, 25)
  val labelSize = new Dimension(300, 25)
  val numberFieldSize = new Dimension(60, 25)

  val kLabel = new Label("Parámetro K (Peso del Control, [0, 1])")
  val kField = new TextField("0.8")

  val tempoLabel = new Label("Tempo (BPM)")
  val tempoField = new TextField("120")

  val normLabel = new Label("Normalización de las notas (octava inicial)")
  val normField = new ComboBox(List(0, 2, 4, 6, 8))
  normField.peer.setSelectedIndex(2)

  val noteDistanceLabel = new Label("Distancia máxima entre la nota de salida y el control")
  val noteDistanceField = new ComboBox(1 to 25)
  noteDistanceField.peer.setSelectedIndex(5)

  val durationDistanceLabel = new Label("Distancia máxima entre la duración de salida y el control")
  val durationDistanceField = new ComboBox(1 to 8)
  durationDistanceField.peer.setSelectedIndex(2)

  val programChangeLabel = new Label("Instrumento (MIDI Program Change)")
  val programChangeField = new ComboBox(0 to 127)

  val saveButton = Button("Guardar Cambios"){
    kMMGUI.kController ! UpdateK(kField.text.toDouble)
    kMMGUI.conductor ! UpdateTempo(tempoField.text.toInt)
    kMMGUI.kController ! UpdateMaxNoteDistanceToControl(noteDistanceField.peer.getSelectedItem.toString.toByte)
    kMMGUI.conductor ! UpdateOutputNormalization((normField.peer.getSelectedItem.toString.toInt * 12).toByte)
    kMMGUI.kController ! UpdateMaxDurationDistanceToControl(durationDistanceField.peer.getSelectedItem.toString.toByte)
    kMMGUI.conductor ! UpdateProgramChange(programChangeField.peer.getSelectedItem.toString.toByte)
    dispose()
  }

  val settingsPanel = new BoxPanel(Orientation.Vertical) {
    contents += Swing.HStrut(10)
    contents += new GridPanel(1, 2) {
      contents += kLabel
      contents += kField
    }
    contents += Swing.HStrut(5)
    contents += new GridPanel(1, 2) {
      contents += tempoLabel
      contents += tempoField
    }
    contents += Swing.HStrut(5)
    contents += new GridPanel(1, 2) {
      contents += normLabel
      contents += normField
    }
    contents += Swing.HStrut(5)
    contents += new GridPanel(1, 2) {
      contents += noteDistanceLabel
      contents += noteDistanceField
    }
    contents += Swing.HStrut(5)
    contents += new GridPanel(1, 2) {
      contents += durationDistanceLabel
      contents += durationDistanceField
    }
    contents += Swing.HStrut(5)
    contents += new GridPanel(1, 2) {
      contents += programChangeLabel
      contents += programChangeField
    }
    contents += Swing.HStrut(10)
    contents += new GridPanel(1, 3) {
      contents += new Label
      contents += saveButton
      contents += new Label
    }
  }

  parentPanel.add(settingsPanel.peer)
  add(parentPanel)

  def receive: Receive = {
    case SetVisible => setVisible(true)
  }

}
