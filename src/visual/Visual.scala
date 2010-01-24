package lb.visual


import org.jfree.data.xy.XYZDataset
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.GrayPaintScale
import org.jfree.chart.JFreeChart

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

  private lazy val dataset = MatrixDataset( matrix )

  private lazy val minMax: (Double,Double) = {
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
  private lazy val chart = {
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
