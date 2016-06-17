package TFM.kMarkovMelodyGenerator.charts

/**
  * Created by diego on 30/05/16.
  */

import java.awt.geom.Rectangle2D
import java.awt.{BasicStroke, Color}
import javax.swing.{JFrame, JPanel}

import TFM.CommProtocol.{ChartPanelRef, SetVisible, TransitionsList, UpdateCoords}
import TFM.kMarkovMelodyGenerator.kMMGUI
import akka.actor.Actor
import org.jfree.chart._
import org.jfree.chart.annotations.XYShapeAnnotation
import org.jfree.chart.plot.{IntervalMarker, PlotOrientation, XYPlot}
import org.jfree.data.xy.{XYDataset, XYSeries, XYSeriesCollection}
import org.jfree.ui.Layer

import scala.collection.mutable.ListBuffer
import scala.swing._

class JoystickChart extends JFrame with Actor{

  var manualControl: Boolean = true //TODO - add possibility of changing control type from GUI

  var lastX: Double = 0.5
  var lastY: Double = 0.5

  val durations = List(1, 2, 3, 4, 6, 8, 12, 16)
  val transitionsAnnotations = ListBuffer.empty[XYShapeAnnotation]

  setTitle("Joystick Position")
  setSize(new Dimension(700, 500))
  setLocation(900, 0)
  val parentPanel = new JPanel()
  val chartPanel = createChartPanel
  chartPanel.setDomainZoomable(false)
  chartPanel.setRangeZoomable(false)

  chartPanel.addChartMouseListener(new ChartMouseListener() {

    def chartMouseClicked(event: ChartMouseEvent ){
      getMouseCoords(event)
    }

    def chartMouseMoved(event: ChartMouseEvent ){
      //getMouseCoords(event)
    }
  })

  parentPanel.add(chartPanel)
  add(parentPanel)
  //setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  //frame.setVisible(true)

  def getMouseCoords(mouseChartEvent: ChartMouseEvent) = {
    if (manualControl) {
      val plot = chartPanel.getChart().getXYPlot()
      val p = chartPanel.translateScreenToJava2D(mouseChartEvent.getTrigger().getPoint())
      kMMGUI.addOutput(p.getX.toString + " - " + p.getY.toString)
      val plotArea = chartPanel.getScreenDataArea()
      lastX = plot.getDomainAxis().java2DToValue(p.getX(), plotArea, plot.getDomainAxisEdge())
      lastY = plot.getRangeAxis().java2DToValue(p.getY(), plotArea, plot.getRangeAxisEdge())
      //val coords = (chartX, chartY)
      kMMGUI.conductor ! UpdateCoords((lastX, lastY))
      plot.setDataset(createDatasetFromPoint(lastX, lastY))
    }
  }

  def createChartPanel: ChartPanel = {
    val jfreechart = ChartFactory.createScatterPlot("Joystick Position", "X", "Y", createDatasetFromPoint(lastX, lastY), PlotOrientation.VERTICAL, true, true, false)
    val xyPlot = jfreechart.getXYPlot()
    xyPlot.setDomainCrosshairVisible(true)
    xyPlot.setRangeCrosshairVisible(true)
    val renderer = xyPlot.getRenderer();
    renderer.setSeriesPaint(0, Color.blue)
    val domain = xyPlot.getDomainAxis()
    domain.setRange(0.00, 1.00)
    domain.setVerticalTickLabels(true)
    val range = xyPlot.getRangeAxis()
    val target: IntervalMarker = new IntervalMarker(0.2, 0.6)
    target.setLabel("Target Domain")
    //xyPlot.addDomainMarker(target, Layer.BACKGROUND)
    //xyPlot.addRangeMarker(target, Layer.BACKGROUND)
    range.setRange(0.0, 1.0)
    //range.setTickUnit(new NumberTickUnit(0.1));
    new ChartPanel(jfreechart)
  }

  def addAnnotation(annotation: XYShapeAnnotation) = {
    chartPanel.getChart.getXYPlot.getRenderer.addAnnotation(annotation, Layer.BACKGROUND)
  }

  def removeAnnotation(annotation: XYShapeAnnotation) = {
    chartPanel.getChart.getXYPlot.getRenderer.removeAnnotation(annotation)
  }

  def createTransitionAnnotation(note: Int, duration: Int, alpha: Double) = {
    val xPosition = durations.indexOf(duration) * (1.0f/durations.size.toFloat)
    val yPosition = note * (1.0f/25.0f)
    new XYShapeAnnotation(
      new Rectangle2D.Double(xPosition, yPosition, 1.0f/8.0f, 1.0f/24.0f),
      new BasicStroke(0.0f),
      new Color(51, 204, 51, 0),
      new Color(51, 204, 51, Math.min(Math.max((alpha * 1.5 * 255).toInt, 25), 255))
    )
  }

  //annotation example
  def addAnnotation(plot: XYPlot) = {
    plot.getRenderer.addAnnotation(
      new XYShapeAnnotation(
        new Rectangle2D.Double(0.2, 0.6, 0.05, 0.03),
        new BasicStroke(1.0f),
        Color.blue,
        Color.yellow
      ),
      Layer.BACKGROUND
    )
  }

  def refreshChart(coords: (Double, Double)) = chartPanel.getChart.getXYPlot().setDataset(createDatasetFromPoint(coords._1, coords._2))

  def createDatasetFromPoint(x: Double, y: Double): XYDataset = {
    var drawX = x
    var drawY = y
    if (x > 1) drawX = 1
    if (x < 0) drawX = 0
    if (y > 1) drawY = 1
    if (y < 0) drawY = 0
    val xySeriesCollection = new XYSeriesCollection()
    val series = new XYSeries("Current Position")
    series.add(x, y)
    xySeriesCollection.addSeries(series)
    xySeriesCollection
  }

  def drawPossibleTransitions(list: List[((Int, Int), Double)]) = {
    transitionsAnnotations.foreach(removeAnnotation(_))
    transitionsAnnotations.clear()
    list.foreach{
      case transition: ((Int, Int), Double) =>
        transitionsAnnotations += createTransitionAnnotation(transition._1._1, transition._1._2, transition._2)
    }
    transitionsAnnotations.foreach(addAnnotation(_))
  }

  def receive: Receive = {
    case SetVisible => setVisible(true)
    case ChartPanelRef(chartPanel: ChartPanel) => parentPanel.add(chartPanel)
    case UpdateCoords(coords) => if(!manualControl) refreshChart(coords)
    case TransitionsList(list: List[((Int, Int), Double)]) => drawPossibleTransitions(list)
    case _ ⇒ println("JoystickChart received unknown message")
  }

}
