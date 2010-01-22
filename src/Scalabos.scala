package lb

// import lb._
import lb.dyn._
import lb.units._

object Timer {
  var start:Long = 0L
  var end:Long = 0L
  def go = {
    System.currentTimeMillis
  }
  def stop = {
    System.currentTimeMillis
  }
}

object Hello {

  def main( args: Array[String] ) : Unit = {
    println("The first Scala lattice Boltzmann Solver (Scalabos) code EVER!!!")
    val physLength = 1.0
    val lbLength   = 100
    val physVel    = 1.0
    val lbVel      = 0.01
    val Re         = 1.0
    val lx         = 1.0
    val ly         = 1.0
    
    val units = new UnitsConverter(D2Q9, Re, physVel,lbVel,physLength,lbLength,lx,ly)
    
    val lattice = new Lattice2D( D2Q9, units.nX, units.nY,new BGKdynamics(D2Q9,units.omega) )
    
    val maxT = 100
    
    val begin = Timer.go
    
    for (iT <- 0 until maxT) { lattice.collideAndStream }
      
    val end = Timer.stop
    
    println("MSUPS = ",maxT*(units.nX*units.nY)/ (end-begin) / 1.0e3)
    println("Total Time = ",(end-begin)/1000.0)
    
  }
}
