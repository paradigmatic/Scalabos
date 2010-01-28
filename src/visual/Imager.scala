package lb.visual

import scala.actors.Actor._

class Imager( prefix: String, 
	      data: => Array[Array[Double]] ) 
{

  private val fmt = "%s_%05d.png"
  
  private[this] var i = -1

  def click() {
    i += 1
    actor {
      val filename = fmt.format( prefix, i )
      //println( "CLICK called with filename=" + filename )
      //println( "i="+i )
      Image( data ).saveAs( filename, 600, 400 )
    }
  }


}
