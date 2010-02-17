package lb.select

import lb._
import scala.collection.mutable.HashSet
import scala.collection.Set
import lb.util._

trait Region {
  def indices( lattice: Lattice2D ): Set[(Int,Int)]
  def indices( lattice: MultiLattice2D ): Set[(Int,Int)]
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
  
  def indices( lattice: MultiLattice2D ) =  {
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
  
  def indices( lattice: MultiLattice2D) = {
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
   
   def indices( lattice: MultiLattice2D ) = {
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
   
   def indices( lattice: MultiLattice2D) = {
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
   
   def indices( lattice: MultiLattice2D) = {
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
	
	def resize(size:Int) = new Rectangle(fromX-size,toX+size,fromY-size,toY+size)
	def shift(offset:Dot2D) = new Rectangle(fromX+offset.x,toX+offset.x,fromY+offset.y,toY+offset.y)
	
  def indices( lattice: Lattice2D ) = {
    val set = new HashSet[(Int,Int)]
    for( x <- fromX to toX; y <- fromY to toY ) {
      set + Tuple(x,y)
    }
    set.readOnly
   }
   
   def indices( lattice: MultiLattice2D ) = {
    val set = new HashSet[(Int,Int)]
    for( x <- fromX to toX; y <- fromY to toY ) {
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
  
  def indices( lattice: MultiLattice2D ) = {
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
  
  def indices( lattice: MultiLattice2D ) = {
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
      ind => { /*println(ind._1+" "+ind._2); */f( lattice( ind._1, ind._2 ) )}
    }
  }

  lazy val elements = {
    var lst = List[Cell]()
    foreach( lst ::= _ )
    lst.elements
  }
}

class ComplexSelection[R <: Region]( lattice: Lattice2D, region: R )  extends Selection[R](lattice, region )

class RectangularSelection( lattice: Lattice2D, rect: Rectangle )
    extends Selection[Rectangle]( lattice, rect ) {

  def apply( x: Int, y: Int ) = lattice( x + rect.fromX, y + rect.fromY )

}

abstract class MultiSelection[R <: Region]( val lattice: MultiLattice2D, val region: R )
    extends Iterable[Cell] {
 
 lazy val indices = region.indices( lattice )

  def foreach( f: (Int, Int, Cell) => Unit ) {
    indices.foreach {
      ind => f( ind._1, ind._2, lattice( ind._1, ind._2 ) )
    }
  }

  override def foreach( f: (Cell) => Unit ) {
    indices.foreach { 
      ind => f( lattice( ind._1, ind._2 ) ) 
    }
  }

  lazy val elements = {
    var lst = List[Cell]()
    foreach( lst ::= _ )
    lst.elements
  }
}

class MultiComplexSelection[R <: Region]( lattice: MultiLattice2D, region: R )  extends MultiSelection[R](lattice, region )

class MultiRectangularSelection( lattice: MultiLattice2D, rect: Rectangle )
    extends MultiSelection[Rectangle]( lattice, rect ) {

  def apply( x: Int, y: Int ) = lattice( x + rect.fromX, y + rect.fromY )

}

object Domains2D {

	def getDomainIntersection(domain1:Rectangle, domain2:Rectangle) = {
		// print domain1.fromX, domain1.toX, domain1.fromY, domain1.toY, domain2.fromX, domain2.toX, domain2.fromY, domain2.toY
		if ((domain1.fromX > domain2.toX) || (domain2.fromX > domain1.toX) || 
				(domain1.fromY > domain2.toY) || (domain2.fromY > domain1.toY)) None
				// print domain1.fromX, domain1.toX, domain1.fromY, domain1.toY, domain2.fromX, domain2.toX, domain2.fromY, domain2.toY
				// print "No domain intersection."
				// we tested if domain do have an intersection
		
		// print "Domain intersection !!!"
		// print domain1.fromX, domain1.toX, domain1.fromY, domain1.toY, domain2.fromX, domain2.toX, domain2.fromY, domain2.toY
		val intersection = {
			new Rectangle({if (domain1.fromX >= domain2.fromX)  domain1.fromX
								 else domain2.fromX},
								{if (domain1.toX >= domain2.toX) domain2.toX
								 else domain1.toX}, 
								{if (domain1.fromY >= domain2.fromY) domain1.fromY
								 else domain2.fromY},
								{if (domain1.toY >= domain2.toY) domain2.toY
								 else domain1.toY})
		}

		intersection
	}

}

