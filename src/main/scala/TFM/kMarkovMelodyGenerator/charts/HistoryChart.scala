package TFM.kMarkovMelodyGenerator.charts

import java.awt.geom.Rectangle2D
import java.awt.{BasicStroke, BorderLayout, Color, Dimension}
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{JFrame, JPanel, JSlider}

import TFM.CommProtocol._
import TFM.util.SlidingXYDataset
import akka.actor.{Actor, ActorRef}
import org.jfree.chart.annotations.XYShapeAnnotation
import org.jfree.chart.axis.{AxisLocation, NumberAxis, NumberTickUnit}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.StandardXYItemRenderer
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.Layer

/**
  * Created by diego on 9/06/16.
  */
class HistoryChart extends JFrame with Actor with ChangeListener{

  setTitle("Histograma del Control")
  setSize(new Dimension(1200, 400))
  setLocation(500, 660)
  val parentPanel = new JPanel(new BorderLayout())

  var firstIndex: Int = 0
  val WINDOW: Int = 16 * 8
  val ANNOTATION_HEIGHT = 1

  val controlNotesCollection = new XYSeriesCollection()
  val controlNotesData = new XYSeries("Nota")
  controlNotesData.add(0, 11)
  controlNotesData.add(1, 11)
  controlNotesCollection.addSeries(controlNotesData)
  var slidingNotesCollection = new SlidingXYDataset(controlNotesCollection, firstIndex, WINDOW)

  val controlDurationsCollection = new XYSeriesCollection()
  val controlDurationsData = new XYSeries("Duración")
  controlDurationsData.add(0, 4)
  controlDurationsData.add(1, 4)
  controlDurationsCollection.addSeries(controlDurationsData)
  var slidingDurationsCollection = new SlidingXYDataset(controlDurationsCollection, firstIndex, WINDOW)

  val renderer = new StandardXYItemRenderer()
  val noteAxis = new NumberAxis("Control Note")
  noteAxis.setRange(-1.6, 23.6)
  noteAxis.setTickUnit(new NumberTickUnit(1))
  val plot = new XYPlot(slidingNotesCollection, null, noteAxis, renderer)
  plot.setRangeAxisLocation(AxisLocation.BOTTOM_OR_LEFT)
  val domainAxis = new NumberAxis("Midi Tick")
  domainAxis.setTickUnit(new NumberTickUnit(4))
  domainAxis.setRange(0, WINDOW)
  plot.setDomainAxis(domainAxis)

  plot.setDataset(1, slidingDurationsCollection)
  val durationAxis = new NumberAxis("Control Duration")
  durationAxis.setTickUnit(new NumberTickUnit(1))
  durationAxis.setRange(0.8, 16.2)
  durationAxis.setTickUnit(new NumberTickUnit(1))
  plot.setRangeAxis(1, durationAxis)
  plot.setRangeAxisLocation(1, AxisLocation.BOTTOM_OR_RIGHT)
  plot.setRenderer(1, new StandardXYItemRenderer())
  plot.mapDatasetToRangeAxis(1, 1)

  val chart = new JFreeChart("Histograma del Control", JFreeChart.DEFAULT_TITLE_FONT, plot, true)
  val chartPanel = new ChartPanel(chart)
  chartPanel.setPreferredSize(new Dimension(600, 300))
  chartPanel.setDomainZoomable(false)
  chartPanel.setRangeZoomable(false)

  var slider = new JSlider(0, WINDOW, 0)
  slider.setPaintLabels(true)
  slider.setPaintTicks(true)
  slider.setMajorTickSpacing(WINDOW)
  slider.addChangeListener(this)
  slider.setVisible(false)

  add(chartPanel)
  parentPanel.add(slider)
  add(parentPanel, BorderLayout.SOUTH)

  addAnnotation(createNoteAnnotation(11, 0, 4))

  def addAnnotation(annotation: XYShapeAnnotation) = {
    renderer.addAnnotation(annotation, Layer.BACKGROUND)
  }

  def createNoteAnnotation(note: Double, tick: Int, duration: Int) = {
    val yPosition: Double = note - (ANNOTATION_HEIGHT/2.0f)
    new XYShapeAnnotation(
      new Rectangle2D.Double(tick, yPosition, duration, ANNOTATION_HEIGHT),
      new BasicStroke(1.0f),
      Color.black,
      Color.yellow
    )
  }

  def createCutAnnotation(note: Double, tick: Int) = {
    val yPosition: Double = note - (ANNOTATION_HEIGHT/2.0f)
    new XYShapeAnnotation(
      new Rectangle2D.Double(tick, yPosition - 0.2, 0.5, ANNOTATION_HEIGHT + 0.4),
      new BasicStroke(0.0f),
      Color.blue,
      Color.red
    )
  }

  def removeAnnotation(annotation: XYShapeAnnotation) = {
    renderer.removeAnnotation(annotation)
  }

  def stateChanged(event: ChangeEvent) {
    firstIndex = slider.getValue()
    slidingNotesCollection.setFirstItemIndex(firstIndex)
    slidingDurationsCollection.setFirstItemIndex(firstIndex)
    domainAxis.setRange(firstIndex, firstIndex + WINDOW)
  }

  def updateDomain(tick: Int) = {
    val newFirst = tick - WINDOW
    slider.setMaximum(newFirst)
    firstIndex = newFirst
    slider.setValue(firstIndex)
    if (!slider.isVisible) slider.setVisible(true)
    slidingNotesCollection.setFirstItemIndex(firstIndex)
    slidingDurationsCollection.setFirstItemIndex(firstIndex)
    domainAxis.setRange(firstIndex, firstIndex + WINDOW)
  }

  def receive: Receive = {
    case SetVisible => setVisible(true)
    case UpdateHistogram(controlNote: Int, controlDuration: Int, tick: Long) => {
      controlNotesData.add(tick, controlNote)
      controlDurationsData.add(tick, controlDuration)
      if (tick > WINDOW) {
        updateDomain(tick.toInt)
      }
    }
    case DrawNote(tick: Long, note: Int, duration: Int) => addAnnotation(createNoteAnnotation(note, tick.toInt, duration))
    case DrawNoteCut(tick: Long, note: Int) => addAnnotation(createCutAnnotation(note, tick.toInt))
    case _ ⇒ println("HistoryChart received unknown message")
  }

}
