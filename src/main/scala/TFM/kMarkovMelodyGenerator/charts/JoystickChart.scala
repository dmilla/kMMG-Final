package TFM.kMarkovMelodyGenerator.charts

/**
  * Created by diego on 30/05/16.
  */

import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{BasicStroke, BorderLayout, Color}
import javax.swing.{JFrame, JPanel}

import TFM.CommProtocol._
import TFM.kMarkovMelodyGenerator.kMMGUI
import akka.actor.Actor
import org.jfree.chart._
import org.jfree.chart.annotations.XYShapeAnnotation
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYDataset, XYSeries, XYSeriesCollection}
import org.jfree.ui.Layer

import scala.collection.mutable.ListBuffer
import scala.swing._

class JoystickChart extends JFrame with Actor{

  var manualControl: Boolean = true
  //var currentStateTransitions: List[((Int, Int), Double)] = List.empty[((Int, Int), Double)]
  var lastX: Double = 0.5
  var lastY: Double = 0.5
  var controlRangeAnnotation = createControlRangeAnnotation

  lazy val durations = List(1, 2, 3, 4, 6, 8, 12, 16)
  val transitionsAnnotations = ListBuffer.empty[XYShapeAnnotation]

  setTitle("Posición del Joystick")
  setSize(new Dimension(800, 630))
  setLocation(900, 0)
  val chartPanel = createChartPanel
  chartPanel.setDomainZoomable(false)
  chartPanel.setRangeZoomable(false)
  chartPanel.setPreferredSize(new Dimension(800, 600))
  addAnnotation(controlRangeAnnotation)

  chartPanel.addChartMouseListener(new ChartMouseListener() {

    def chartMouseClicked(event: ChartMouseEvent ){
      getMouseCoords(event)
    }

    def chartMouseMoved(event: ChartMouseEvent ){
    }

  })

  add(chartPanel)

  def getMouseCoords(mouseChartEvent: ChartMouseEvent): Boolean = {
    if (manualControl) {
      val plot = chartPanel.getChart.getXYPlot
      val p = chartPanel.translateScreenToJava2D(mouseChartEvent.getTrigger.getPoint)
      //kMMGUI.addOutput(p.getX.toString + " - " + p.getY.toString)
      val plotArea = chartPanel.getScreenDataArea
      val x = plot.getDomainAxis.java2DToValue(p.getX, plotArea, plot.getDomainAxisEdge)
      val y = plot.getRangeAxis.java2DToValue(p.getY, plotArea, plot.getRangeAxisEdge)
      kMMGUI.conductor ! UpdateCoords((x, y))
      refreshChart(x, y)
      true
    } else false
  }

  def createChartPanel: ChartPanel = {
    val jfreechart = ChartFactory.createScatterPlot("Posición del Joystick", "X", "Y", createDatasetFromPoint(lastX, lastY), PlotOrientation.VERTICAL, true, true, false)
    val xyPlot = jfreechart.getXYPlot
    xyPlot.setDomainCrosshairVisible(true)
    xyPlot.setRangeCrosshairVisible(true)
    val renderer = xyPlot.getRenderer
    renderer.setSeriesPaint(0, Color.blue)
    val domain = xyPlot.getDomainAxis
    domain.setRange(0.00, 1.00)
    domain.setLabel("Duración (Semicorchea --> Redonda)")
    val range = xyPlot.getRangeAxis
    range.setRange(0.0, 1.0)
    range.setLabel("Nota (Silencio -> Grave -> Agudo)")
    new ChartPanel(jfreechart)
  }

  def addAnnotation(annotation: XYShapeAnnotation) = {
    chartPanel.getChart.getXYPlot.getRenderer.addAnnotation(annotation, Layer.BACKGROUND)
  }

  def removeAnnotation(annotation: XYShapeAnnotation) = {
    chartPanel.getChart.getXYPlot.getRenderer.removeAnnotation(annotation)
  }

  def createTransitionAnnotation(note: Int, duration: Int, probability: Double) = {
    val xPosition = durations.indexOf(duration) * (1.0f/durations.size.toFloat)
    val yPosition = (note + 1) * 0.04 // adapted for -1 as silence
    val alpha = 108
    val color: Color = probability match {
      case x if x < 0.02 => new Color(206, 0, 0, alpha)
      case x if x < 0.05 => new Color(206, 108, 0, alpha)
      case x if x < 0.1 => new Color(206, 206, 0, alpha)
      case x if x <= 1 => new Color(53, 206, 53, alpha)
      case _ => Color.white
    }
    //val oldColor = new Color(51, 204, 51, Math.min(Math.max((probability * 1.5 * 255).toInt, 25), 255))
    new XYShapeAnnotation(
      new Rectangle2D.Double(xPosition, yPosition, 0.125, 0.04),
      new BasicStroke(0.0f),
      color,
      color
    )
  }

  def createControlRangeAnnotation = {
    val note = (24 * lastY).round.toInt - 1
    val duration = durations((7 * lastX).round.toInt)
    val xPosition = durations.indexOf(duration) * (1.0f/durations.size.toFloat)
    val yPosition = (note + 1) * 0.04 // adapted for -1 as silence
    new XYShapeAnnotation(
      new Ellipse2D.Double(xPosition - 0.0625, yPosition - 0.02, 0.25, 0.08),
      new BasicStroke(0.8f),
      Color.black,
      new Color(53, 206, 53, 26)
    )
  }

  /*def addAnnotation(plot: XYPlot) = {
    plot.getRenderer.addAnnotation(
      new XYShapeAnnotation(
        new Rectangle2D.Double(0.2, 0.6, 0.05, 0.03),
        new BasicStroke(1.0f),
        Color.blue,
        Color.yellow
      ),
      Layer.BACKGROUND
    )
  }*/

  def refreshChart(coords: (Double, Double)) = {
    lastX = coords._1
    lastY = coords._2
    chartPanel.getChart.getXYPlot.setDataset(createDatasetFromPoint(coords._1, coords._2))
    removeAnnotation(controlRangeAnnotation)
    controlRangeAnnotation = createControlRangeAnnotation
    addAnnotation(controlRangeAnnotation)
    /*val controlDurationIndex = (7 * coords._1).round.toInt // Normalized to 8 possible durations
    val controlDuration = durations(controlDurationIndex)
    val controlNote = (23 * coords._2).round.toInt
    var lowNearestNote = -1
    var highNearestNote = 23
    var shortNearestDuration = 1
    var longNearestDuration = 16
    currentStateTransitions.foreach{
      case((note: Int, duration: Int), prob: Double) =>
        val noteDistance = note - controlNote
        val durationDistance = duration - controlDuration
        if (noteDistance < 0 && noteDistance >= lowNearestNote - controlNote) lowNearestNote = note
        if (noteDistance > 0 && noteDistance <= highNearestNote - controlNote) highNearestNote = note
        if (durationDistance < 0 && durationDistance >= shortNearestDuration - controlDuration) shortNearestDuration = duration
        if (durationDistance > 0 && durationDistance <= longNearestDuration) longNearestDuration = duration
    }*/
  }

  def createDatasetFromPoint(x: Double, y: Double): XYDataset = {
    var drawX = x
    var drawY = y
    if (x > 1) drawX = 1
    if (x < 0) drawX = 0
    if (y > 1) drawY = 1
    if (y < 0) drawY = 0
    val xySeriesCollection = new XYSeriesCollection()
    val series = new XYSeries("Posición Actual")
    series.add(x, y)
    xySeriesCollection.addSeries(series)
    xySeriesCollection
  }

  def drawPossibleTransitions(list: List[((Int, Int), Double)]) = {
    transitionsAnnotations.foreach(removeAnnotation(_))
    transitionsAnnotations.clear()
    //currentStateTransitions = list
    list.foreach{
      case transition: ((Int, Int), Double) =>
        transitionsAnnotations += createTransitionAnnotation(transition._1._1, transition._1._2, transition._2)
    }
    transitionsAnnotations.foreach(addAnnotation(_))
  }

  def receive: Receive = {
    case SetVisible => setVisible(true)
    case EndManualControlRequest => manualControl = false
    case UpdateCoords(coords) =>
      try {
        if(!manualControl) refreshChart(coords)
      } catch {
        case e: Exception => println("JoystickChart - Excepción actualizando posición: " + e)
      }
    case TransitionsList(list: List[((Int, Int), Double)]) =>
      try {
        drawPossibleTransitions(list)
      } catch {
        case e: Exception => println("JoystickChart - Excepción dibujando transiciones: " + e)
      }
    case _ ⇒ println("JoystickChart received unknown message")
  }

}
