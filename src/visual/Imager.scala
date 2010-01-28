package lb.visual

import scala.actors.Actor._

class Imager( val lattice: Lattice2D[_], 
	      val prefix: String, 
	      val producer: (Lattice2D[_]) => Array[Array[Double]] ) 
{

  private val fmt = "%s_%05d.png"
  
  private var i = 0

  def click() {
    actor {
      val filename = fmt.format( prefix, i )
      //println( "CLICK called with filename=" + filename )
      i += 1
      //println( "i="+i )
      Image( producer( lattice ) ).saveAs( filename, 600, 400 )
    }
  }


}
