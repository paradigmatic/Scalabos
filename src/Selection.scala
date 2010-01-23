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


//TODO: decide convention about y direction

case object NorthWall  extends Region {
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) = {
    val y = lattice.nY - 1
    for( x <- 0 until lattice.nX ) f( lattice(x,y) ) 
     
   }
}

case object SouthWall  extends Region {
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) = {
    val y = 0
    for( x <- 0 until lattice.nX ) f( lattice(x,y) ) 
     
   }
}

case object WestWall extends Region {
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) = {
    val x = 0
    for( y <- 0 until lattice.nY ) f( lattice(x,y) ) 
     
   }
}

case object EastWall extends Region {
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) = {
    val x = lattice.nX - 1
    for( y <- 0 until lattice.nY ) f( lattice(x,y) ) 
     
   }
}


case class Rectangle( val fromX: Int, val toX: Int,
                      val fromY: Int, val toY: Int) extends Region {
  //TODO: check bounds
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) = {
     for( x <- fromX until toX; y <- fromY until toY ) f( lattice(x,y) ) 
     
   }
}

case class Where( val predicate: (Int,Int) => Boolean ) extends Region {
  def foreach[D <: Descriptor]( lattice: Lattice2D[D], f: (Cell[D]) => Unit ) = {
    for( x <- 0 until lattice.nX; 
         y <- 0 until lattice.nY if predicate(x,y) ) f( lattice(x,y) )   
  }
}


class Selection[D <: Descriptor]( lattice: Lattice2D[D], regions: Region ) {

  def foreach( f: (Cell[D]) => Unit ) {
    regions.foreach(lattice,f) 
  }
  
}
