package lb

import lb.dyn._


abstract class Lattice2D( val nX: Int, val nY: Int,
		 val defaultDynamics: Dynamics )  {

  private val grid = {
    val g = new Array[Array[Cell]](nX,nY)
    for( iX <- 0 until nX; iY <- 0 until nY ) {
      g(iX)(iY) = createCell
    }
    g
  }

  def collide() { }
 
  def stream() { }

  def collideAndStream() = { collide; stream }

  def apply( x: Int, y: Int ) = grid(x)(y)

  def createCell(): Cell
  
 
}


class D2Q9Lattice( nX: Int, nY: Int,
		   defaultDynamics: Dynamics)
extends Lattice2D( nX, nY, defaultDynamics) {

  def createCell() = new Cell(defaultDynamics) with D2Q9

}
