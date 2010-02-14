package lb.desc

trait Descriptor {

  val q: Int 									// The number of velocities
  val d: Int 									// The number physical dimensions of a lattice
  val n: Int 									// The number of independet components of a symmetric tensor
  val c: Array[Array[Int]] 		// velocities of the lattice
  val t: Array[Double] 				// Weights associated with the lattice velocities
  val invCs2: Double 					// Sound speed squared inverse
  lazy val cs2 = 1.0 / invCs2 // sound speed squared
  override lazy val toString = "D"+d+"Q"+q
  lazy val popIndices = (0 until q).toList // Frequently used range over all population indicies
  lazy val dimIndices = (0 until d).toList // Frequently used range over all dimension indicies
  
  val opposite: Array[Int]
  val vicinity: Int
}
