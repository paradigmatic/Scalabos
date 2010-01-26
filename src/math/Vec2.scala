package lb.math

trait VecDouble {
  def size(): Int
  def length(): Int
  def norm(): Double
  def norm2(): Double
  def apply( index: Int ): Double
  def toArray(): Array[Double]
}

trait Vec2Double extends VecDouble {
  
  def x(): Double
  def y(): Double

}

class Vec2DoubleImpl( val x: Double, val y: Double ) extends Vec2Double {

  val size = 2
  val length = size

  lazy val norm2 = x*x + y*y
  lazy val norm = Math.sqrt( norm2 )

  def *( that: Vec2Double ) = this.x*that.x + this.y*that.y
  
  def *( k: Double ) = new Vec2DoubleImpl( k*x, k*y )

  def /( k: Double ) = new Vec2DoubleImpl( x/k, y/k )

  def apply( i: Int ) = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IllegalArgumentException("Out of bond")
  }

  def toArray = Array( x, y )

  override lazy val toString = "Vec2Double(" + x + ", " + y + ")"

}
