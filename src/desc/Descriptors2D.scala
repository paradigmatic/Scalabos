package lb.desc

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
  
  val opposite = Array(0, 5, 6, 7, 8, 1, 2, 3, 4)
  
  val vicinity = 1

}
