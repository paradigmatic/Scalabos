package lb.select

import lb._

trait Region {
   def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ): Unit
  def and( other: Region ):Region = MultiRegion( this, other )
  def &( other: Region )  = and( other )
}

case class MultiRegion( r1: Region, r2: Region ) extends Region {

  private var regions = List( r1, r2  )
  
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) {
    regions.foreach( _.foreach( lattice, f ) )
  }
  
  override def and( other: Region ) = {
    regions ::= other
    this
  }

}

case class Rectangle( val fromX: Int, val toX: Int,
                      val fromY: Int, val toY: Int) extends Region {
  //TODO: check bounds
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) = {
     for( x <- fromX until toX; y <- fromY until toY ) f( lattice(x,y) ) 
     
   }
}


class Selection[D <: Descriptor]( lattice: Lattice2D[D], regions: Region ) {

  def foreach( f: (Cell[D]) => Unit ) {
    regions.foreach(lattice,f) 
  }
  
}
