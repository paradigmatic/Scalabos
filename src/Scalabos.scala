package lb

// import lb._
import lb.dyn._
import lb.units._
import lb.simSetup._
import lb.util._
import lb.visual._
import lb.select._

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

class TaylorGreen2D[T <: Descriptor](val units:UnitsConverter[T], val m:Int, val n:Int) {
  
  def density(iX:Int, iY:Int) = {
    lazy val pi = 4.0 * Math.atan(1.0)
    val x = iX.toDouble / (units.nX-1).toDouble
    val y = iY.toDouble / (units.nY-1).toDouble
    
    val pressure = -Doubles.sqr(pi* units.deltaT / units.deltaX) *(n*n*Math.cos(4.0*pi*m*x)+m*m*Math.cos(4.0*pi*n*y))
    1.0+pressure * 3.0
  }
  
  def velocity(iX:Int, iY:Int) : Array[Double] = {
    lazy val pi = 4.0 * Math.atan(1.0)
    val x = iX.toDouble / (units.nX-1).toDouble
    val y = iY.toDouble / (units.nY-1).toDouble
    
    val u = new Array[Double](2)
    
    u(0) = units.lbVel * (- n * 2.0 * pi * Math.sin(n * y * 2.0 * pi) * Math.cos(m * x * 2.0 * pi))
    u(1) = units.lbVel * (  m * 2.0 * pi * Math.sin(m * x * 2.0 * pi) * Math.cos(n * y * 2.0 * pi))
    
    u
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
    
    val ini = new TaylorGreen2D(units,1,1)
    
    val allLattice = new Selection(lattice,WholeDomain)
    allLattice.foreach(SimSetup.iniAtEquilibrium(D2Q9,ini.density,ini.velocity)_)
    
    
    val maxT = 100000

      val begin = Timer.go
    
    for (iT <- 0 until maxT) { 
			lattice.collideAndStream 
// 			Image( lattice.map( _.rho ) ).display
		}
      
      val end = Timer.stop

      val msups = 1.0 * maxT * (units.nX*units.nY)/ (end-begin) / 1.0e3

    
      println("MSUPS = " + msups )
      println("Total Time = " + (end-begin)/1000.0 )
      System.gc
      System.gc
  }
}
