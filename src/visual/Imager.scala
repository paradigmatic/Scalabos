package lb.visual

import scala.actors.Actor._

class Imager( val lattice: Lattice2D[_], 
	      val prefix: String, 
	      val producer: (Lattice2D[_]) => Array[Array[Double]] ) 
{

  private val fmt = "%s_%05d.png"
  
  private[this] var i = -1

  def click() {
    i += 1
    actor {
      val filename = fmt.format( prefix, i )
      //println( "CLICK called with filename=" + filename )
      //println( "i="+i )
      Image( producer( lattice ) ).saveAs( filename, 600, 400 )
    }
  }


}
