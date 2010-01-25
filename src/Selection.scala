package lb.select

import lb._

trait Region {
   def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ): Unit
}

case class Rectangle( val fromX: Int, val toX: Int,
                      val fromY: Int, val toY: Int) extends Region {
  //TODO: check bounds
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) = {
    println( "Call foreach of " + toString )
     for( x <- fromX until toX; y <- fromY until toY ) {
       println( x + " : " + y )
       f( lattice(x,y) ) 
     }
   }
}


class Selection[D <: Descriptor]( lattice: Lattice2D[D], regions: List[Region] ) {

  def foreach( f: (Cell[D]) => Unit ) {
    println( regions )
    regions.foreach( _.foreach(lattice,f) )
  }
  
}

class SelectionBuilder {

  private var regions = List[Region]()
  
  def andA( region: Region ) =  {
    regions ::= region
    this
  }

  def in[D <: Descriptor]( lattice: Lattice2D[D] )  =
    new Selection( lattice, regions )

}

object Select {
  def a( region: Region ) = {
    val builder = new SelectionBuilder
    builder.andA( region )
  }
}
