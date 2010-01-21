package lb

trait Descriptor {

  val Q: Int
  val D: Int
  val C: Array[Array[Int]]
  val T: Array[Double]
  val invCs2: Double
  lazy val cs2 = 1.0 / invCs2

}

trait D2Q9 {

  val Q = 9
  val D = 2
  val C = Array( Array(0,0),
		 Array(-1,1), Array(-1,0), Array(-1,-1), Array(0,-1),
		 Array(1,-1), Array(1,0), Array(1,1), Array(0,1) )
  val T = Array( 4.0/9, 
	         1.0/36, 1.0/9, 1.0/36, 1.0/9,
		 1.0/36, 1.0/9, 1.0/36, 1.0/9 )
  val invCs2 = 3.0

}
