package lb.math

abstract class Vec[V,T]( val size: Int ) {
  self: V =>
  def apply( i: Int ): T
  def +( other: V ): V
  def -( other: V ): V
  def unary_-(): V
  def **( other: V): T
  def *( scalar: T ): V
  def /( scalar: T ): V
  def norm2(): Double
  lazy val norm = Math.sqrt( norm2 )
  def toArray(): Array[T]
}

abstract class Vec2[T]( val x: T, val y: T ) extends Vec[Vec2[T],T](2) {
  def apply( i: Int ) = i match {
    case 0 => x
    case 1 => y
    case _ => throw new IllegalArgumentException("Out of bond")
  }
  def toArray() = {
    val ary = new Array[T](2)
    ary(0) = x
    ary(1) = y
    ary
  }
  override lazy val toString = "Vec(" + x + ", " + y + ")"
}

class Vec2Double( x: Double, y: Double ) extends Vec2[Double](x,y){
  def unary_-() = new Vec2Double( -x, -y )
  def +( other: Vec2[Double] ) = new Vec2Double( x + other.x, y + other.y )
  def -( other: Vec2[Double] ) = new Vec2Double( x - other.x, y - other.y )
  def **( other: Vec2[Double] ) = x*other.x + y*other.y
  def *( scalar: Double ) = new Vec2Double( scalar*x, scalar*y )
  def /( scalar: Double ) = new Vec2Double( scalar/x, scalar/y )
  lazy val norm2 = x*x + y*y

} 

object Vec2Double {
  def apply( x: Double, y: Double ) = new Vec2Double( x, y )  
}
    

object VecDemo {

  def main(args: Array[String]) = {
    val v1 = Vec2Double( 0, 2 )
    val v2 = Vec2Double( 1, 3 )
    v1 + v2
  }

}
