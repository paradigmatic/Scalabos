package lb

import lb.dyn._
import lb.util.Util._

abstract class Lattice2D[T <: Descriptor]( val D:T, val nX: Int, val nY: Int,
		 val defaultDynamics: Dynamics[T] )  {

  private val grid = {
    val g = new Array[Array[Cell[T]]](nX,nY)
    for( iX <- 0 until nX; iY <- 0 until nY ) {
      g(iX)(iY) = new Cell(defaultDynamics)
    }
    g
  }

  def collide() = { 
    for (iX <- 0 until nX; iY <- 0 until nY) {
      grid(iX)(iY).collide
      grid(iX)(iY).revert
    }
  }
 
  def stream() {}
//   = {
//     lazy val half = Q/2
//     for (iX <- 0 until nX; iY <- 0 until nY; iPop <- 1 until half+1) {
//       val nextX = (iX + C(iPop)(0) + nX) % nX
//       val nextY = (iY + C(iPop)(1) + nY) % nY
//       
//       val tmp = grid(iX)(iY)(iPop+half)
//       grid(iX)(iY)(iPop+half) = grid(nextX)(nextY)(iPop)
//       self.grid(nextX)(nextY)(iPop) = tmp
//     }
//   }

  def collideAndStream() = { collide; stream }

  def apply( x: Int, y: Int ) = grid(x)(y)
 
}
