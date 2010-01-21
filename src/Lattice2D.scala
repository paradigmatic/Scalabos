package lb

class Lattice2D( val nX: Int, val nY: Int ) {

  private val grid = new Array[Array[Cell]](nX,nY)

  def collide() { }
 
  def stream() { }

  def collideAndStream() = { collide; stream }

  def apply( x: Int, y: Int ) = grid(x)(y)
  

}
