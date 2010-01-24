package lb.visual


import org.jfree.data.xy.XYZDataset
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.GrayPaintScale
import org.jfree.chart.JFreeChart
import org.jfree.chart.ChartPanel
import java.awt.Dimension
import java.io.File
import org.jfree.ui.ApplicationFrame
import org.jfree.chart.ChartUtilities



object MatrixDataset {

  def apply( matrix: Array[Array[Double]] ):XYZDataset = {
    import org.jfree.data.xy.DefaultXYZDataset
    import scala.collection.mutable.ArrayBuffer

    val data = new DefaultXYZDataset
    val xs = new ArrayBuffer[Double]
    val ys = new ArrayBuffer[Double]
    val zs = new ArrayBuffer[Double]
    for( i <- 0 until matrix.size; j <- 0 until matrix(0).size) {
      xs += i
      ys += j
      zs += matrix(i)(j)
    } 
    val series = Array( xs.toArray, ys.toArray, zs.toArray )
    data.addSeries( "lb-data", series)
    data
  }

}

class Image( private val matrix: Array[Array[Double]] ) {

  lazy val dataset = MatrixDataset( matrix )

  lazy val minMax: (Double,Double) = {
    def findMinMax( init: (Double,Double), 
                   ary: Array[Double]): (Double,Double) = {
      ary.foldLeft( init ) { 
        (result,d) => {
          val min = if( d < result._1 ) d else result._1
            val max = if( d > result._2 ) d else result._2
          (min,max)
        } 
      }
    }
    val default = ( java.lang.Double.MAX_VALUE, java.lang.Double.MIN_VALUE )
    matrix.foldLeft( default ) {
      (scores, ary) => findMinMax(scores, ary)
    }
  }

  lazy val chart = {
    val xAxis = new NumberAxis("x Axis")
    val yAxis = new NumberAxis("y Axis")
    val renderer = new XYBlockRenderer
    val paintScale = new GrayPaintScale( minMax._1, minMax._2 )
    renderer setPaintScale paintScale
    renderer setBlockHeight 1.0
    renderer setBlockWidth 1.0
    val plot = new XYPlot(dataset, xAxis, yAxis, renderer )
    val chart = new JFreeChart("Chart Title",JFreeChart.DEFAULT_TITLE_FONT,plot,true)
    chart.removeLegend()
    chart
  }

  lazy val panel = new ChartPanel( chart )

  def display() { new ChartFrame( panel ) }

  def saveAs( filename: String, width: Int, height: Int ) {
    val errorMsg = "File extension not recognized. Currently allowed extension: jpeg, jpg, png"
    val extRegexp = """^.+\.(.+)$""".r
    val extRegexp( ext ) = filename
    ext match {
      case "png" => ChartUtilities.saveChartAsPNG( 
        new File(filename),
        chart,
        width,
        height
      )
      case "jpg" | "jpeg" => ChartUtilities.saveChartAsJPEG( 
        new File(filename),
        chart,
        width,
        height
      )
      case _ => throw new IllegalArgumentException( errorMsg )
    }
  }

  class ChartFrame( val chartPanel: ChartPanel) extends ApplicationFrame("LB") {
    chartPanel.setPreferredSize(new Dimension(640, 480));
    chartPanel.setMouseZoomable(true, false);
    setContentPane(chartPanel);
    pack();
    setVisible(true);  
  }

/*
   private static JFreeChart createChart(XYDataset dataset) {
        NumberAxis scaleAxis = new NumberAxis("New Scale");
        scaleAxis.setUpperBound(250000);
        scaleAxis.setAxisLinePaint(Color.white);
        scaleAxis.setTickMarkPaint(Color.white);
        scaleAxis.setTickLabelFont(new Font("Dialog", Font.PLAIN, 12));
        PaintScaleLegend legend = new PaintScaleLegend(lps,
                scaleAxis);
        legend.setSubdivisionCount(100);
        legend.setAxisLocation(AxisLocation.TOP_OR_RIGHT);
        //legend.setStripOutlineVisible(true);
        legend.setStripOutlinePaint(Color.RED);
        //scaleAxis.setTickLabelFont(new Font("Arial",12,0));
        //legend.setAxisOffset(5.0);
        //legend.setMargin(new RectangleInsets(5, 20, 5, 5));
        legend.setPadding(new RectangleInsets(5, 20, 5, 5));
        legend.setStripWidth(20);
        legend.setPosition(RectangleEdge.LEFT);
        legend.setBackgroundPaint(Color.WHITE);
        chart.addSubtitle(legend);
        chart.setBackgroundPaint(Color.white);
        return chart;
    }
*/
}
