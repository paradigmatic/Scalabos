package lb

trait Descriptor {

  val q: Int 									// The number of velocities
  val d: Int 									// The number physical dimensions of a lattice
  val n: Int 									// The number of independet components of a symmetric tensor
  val c: Array[Array[Int]] 		// velocities of the lattice
  val t: Array[Double] 				// Weights associated with the lattice velocities
  val invCs2: Double 					// Sound speed squared inverse
  lazy val cs2 = 1.0 / invCs2 // sound speed squared
  override lazy val toString = "D"+d+"Q"+q
  lazy val popIndices = (0 until q).toList
  lazy val dimIndices = (0 until d).toList
  
}

object D2Q9 extends Descriptor {
  val q = 9
  val d = 2
  val n = 3
  val c = Array( Array(0,0),
                 Array(-1, 1), Array(-1, 0), Array(-1,-1), Array(0,-1),
                 Array( 1,-1), Array( 1, 0), Array( 1, 1), Array(0, 1) )
  val t = Array( 4.0/9, 
                 1.0/36, 1.0/9, 1.0/36, 1.0/9,
                 1.0/36, 1.0/9, 1.0/36, 1.0/9 )
  val invCs2 = 3.0

}
