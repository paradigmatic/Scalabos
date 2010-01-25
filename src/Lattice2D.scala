package lb

import lb.dyn._
import lb.select._
import lb.util.Util._

class Lattice2D[T <: Descriptor]( val D:T, val nX: Int, val nY: Int,
		 val defaultDynamics: Dynamics[T] )  {

  private val grid = {
    val g = new Array[Array[Cell[T]]](nX,nY)
    for( iX <- 0 until nX; iY <- 0 until nY ) {
      g(iX)(iY) = new Cell(defaultDynamics)
    }
    g
  }
  
  val boundingBox = new Box2D(0,nX-1,0,nY-1)
  
  def apply( x: Int, y: Int ) = grid(x)(y)

  def collide() = { 
    for (iX <- 0 until nX; iY <- 0 until nY) {
      grid(iX)(iY).collide
      // The collided populations are swapped with their opposite direction
      // this step is needed by the streaming step.
      grid(iX)(iY).revert
    }
  }
 
  def stream() = {
    lazy val half = D.q/2
    for (iX <- 0 until nX; iY <- 0 until nY; iPop <- 1 until half+1) {
      // The modulo are used for default periodicity
      val nextX = (iX + D.c(iPop)(0) + nX) % nX
      val nextY = (iY + D.c(iPop)(1) + nY) % nY
      
      // swapping grid(iX,iY,iPop+half) with grid(nextX)(nextY)(iPop)
      val tmp = grid(iX)(iY)(iPop+half)
      grid(iX)(iY)(iPop+half) = grid(nextX)(nextY)(iPop)
      grid(nextX)(nextY)(iPop) = tmp
    }
  }

  def collideAndStream() = { collide; stream }

  def map[A]( f: Cell[T] => A ) = {
    val ary = new Array[Array[A]](nX,nY)
    for( iX <- 0 until nX; iY <- 0 until nY) {
      ary(iX)(iY) = f( grid(iX)(iY) )
    }
    ary
  }

  def select( region: Region ) = new Selection( this, region )

  override lazy val toString = "Lattice("+D+", sizeX="+nX+", sizeY="+nY+")"
}
