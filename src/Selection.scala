package lb.select

import lb._
import scala.collection.mutable.HashSet
import scala.collection.Set

trait Region {
  def indices( lattice: Lattice2D ): Set[(Int,Int)]
  def and( other: Region ):Region = MultiRegion( this, other )
  def &( other: Region )  = and( other )
}

case class MultiRegion( r1: Region, r2: Region ) extends Region {

  private var regions = List( r1, r2  )
  
  def indices( lattice: Lattice2D ) =  {
    val set = new HashSet[(Int,Int)]
    regions.foreach( set ++ _.indices(lattice) )
    set.readOnly
  }
  
  override def and( other: Region ) = {
    regions ::= other
    this
  }

}

//TODO: decide convention about y direction

case object NorthWall  extends Region {
  def indices( lattice: Lattice2D) = {
    val set = new HashSet[(Int,Int)]
    val y = lattice.nY - 1
    for( x <- 0 until lattice.nX ) {
      set + Tuple(x,y)
    }
    set.readOnly
  }
}

case object SouthWall  extends Region {
  def indices( lattice: Lattice2D ) = {
    val set = new HashSet[(Int,Int)]
    val y = 0
    for( x <- 0 until lattice.nX ) {
      set + Tuple(x,y)
    }
    set.readOnly
   }
}

case object WestWall extends Region {
  def indices( lattice: Lattice2D) = {
    val set = new HashSet[(Int,Int)]
    val x = 0
    for( y <- 0 until lattice.nY ) {
      set + Tuple(x,y)
    }
    set.readOnly
   }
}

case object EastWall extends Region {
  def indices( lattice: Lattice2D) = {
    val set = new HashSet[(Int,Int)]
    val x = lattice.nX - 1
    for( y <- 0 until lattice.nY )  {
      set + Tuple(x,y)
    }
    set.readOnly
   }
}

case class Rectangle( val fromX: Int, val toX: Int,
                      val fromY: Int, val toY: Int) extends Region {
  //TODO: check bounds
  def indices( lattice: Lattice2D ) = {
    val set = new HashSet[(Int,Int)]
    for( x <- fromX until toX; y <- fromY until toY ) {
      set + Tuple(x,y)
    }
    set.readOnly
   }
}

case class Where( val predicate: (Int,Int) => Boolean ) extends Region {
  def indices( lattice: Lattice2D ) = {
    val set = new HashSet[(Int,Int)]
    for( x <- 0 until lattice.nX; 
         y <- 0 until lattice.nY if predicate(x,y) ) {
      set + Tuple(x,y)
    }
    set.readOnly  
  }
}

case object WholeDomain extends Region {
  def indices( lattice: Lattice2D ) = {
    val set = new HashSet[(Int,Int)]
    for( x <- 0 until lattice.nX;   
         y <- 0 until lattice.nY  ) {
      set + Tuple(x,y)
    }
    set.readOnly
  }
  override def and( other: Region ) = this
}

abstract class Selection[R <: Region]( val lattice: Lattice2D, val region: R )
    extends Iterable[Cell] {
 
 lazy val indices = region.indices( lattice )

  def foreach( f: (Int, Int, Cell) => Unit ) {
    indices.foreach { 
      ind =>
        f( ind._1, ind._2, lattice( ind._1, ind._2 ) )
    }
  }

  override def foreach( f: (Cell) => Unit ) {
    indices.foreach { 
      ind =>
        f( lattice( ind._1, ind._2 ) )
    }
  }

  lazy val elements = {
    var lst = List[Cell]()
    foreach( lst ::= _ )
    lst.elements
  }
}

class ComplexSelection[R <: Region]( lattice: Lattice2D, region: R )  
extends Selection[R](lattice, region )

class RectangularSelection( lattice: Lattice2D, rect: Rectangle )
    extends Selection[Rectangle]( lattice, rect ) {

  def apply( x: Int, y: Int ) = lattice( x + rect.fromX, y + rect.fromY )

}

