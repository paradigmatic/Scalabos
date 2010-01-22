package lb

trait Descriptor {

  val q: Int
  val d: Int
  val c: Array[Array[Int]]
  val t: Array[Double]
  val invCs2: Double
  lazy val cs2 = 1.0 / invCs2

}

object D2Q9 extends Descriptor{

  val q = 9
  val d = 2
  val c = Array( Array(0,0),
                 Array(-1, 1), Array(-1, 0), Array(-1,-1), Array(0,-1),
                 Array( 1,-1), Array( 1, 0), Array( 1, 1), Array(0, 1) )
  val t = Array( 4.0/9, 
                 1.0/36, 1.0/9, 1.0/36, 1.0/9,
                 1.0/36, 1.0/9, 1.0/36, 1.0/9 )
  val invCs2 = 3.0

}
