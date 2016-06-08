package TFM

/**
  * Created by diego on 30/05/16.
  */

import java.awt.Color
import javax.swing.{JFrame, JPanel}

import TFM.CommProtocol.{UpdateCoords, SetVisible}
import akka.actor.Actor
import org.jfree.chart._
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.xy.{XYDataset, XYSeries, XYSeriesCollection}

import scala.swing._

// TODO - add background color to chart
// TODO - add control notes and durations histogram
class JoystickChart extends JFrame with Actor{

  var manualControl: Boolean = true //TODO - add possibility of changing control type from GUI

  var lastX: Double = 0.5
  var lastY: Double = 0.5

  setTitle("Joystick Position")
  setSize(new Dimension(700, 450))
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
    renderer.setSeriesPaint(0, Color.blue);
    val domain = xyPlot.getDomainAxis()
    domain.setRange(0.00, 1.00)
    domain.setVerticalTickLabels(true)
    val range = xyPlot.getRangeAxis()
    range.setRange(0.0, 1.0);
    //range.setTickUnit(new NumberTickUnit(0.1));
    new ChartPanel(jfreechart)
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

  def receive: Receive = {
    case SetVisible => setVisible(true)
    case UpdateCoords(coords) => if(!manualControl) refreshChart(coords)
    case _ ⇒ println("InputController received unknown message")
  }

}
