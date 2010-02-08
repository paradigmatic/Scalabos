package lb

// import lb._
import lb.dyn._
import lb.units._
import lb.simSetup._
import lb.util._
import lb.visual._
import lb.select._
import lb.dataProcessors2D._
import scala.actors.Actor._


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
  
  def iniGeometry(lattice:Lattice2D, units:UnitsConverter) {
    //     val ini = new TaylorGreen2D(units,1,1)
    //     val ini = new Poiseuille2D(units,0)
    val ini = new Dipole(299.5285375226,0.1,0.0,0.0,0.1,-0.1,units)
    
    dynInterfaces.addVelocityBoundaryConditionOnBoundingBox(lattice)
    
    val applyBoundaryVelocity = SimSetup.defineVelocity(D2Q9,ini.velocity)
    lattice.select(WholeDomain).foreach(applyBoundaryVelocity)
    
//     val applyInitialSetup = SimSetup.iniAtEquilibrium(D2Q9,ini.density,ini.velocity)
//     lattice.select(WholeDomain).foreach(applyInitialSetup)
    
    val applyInitialSetup = SimSetup.iniAtFirstOrder(D2Q9,ini.density,ini.velocity,ini.deviatoricStress)
    lattice.select(WholeDomain).foreach(applyInitialSetup)
  }
  
  def main( args: Array[String] ) : Unit = {
    println("The first Scala lattice Boltzmann Solver (Scalabos) code EVER!!!")
    val physLength = 1.0
    val lbLength   = 50
    val physVel    = 1.0
    val lbVel      = 0.01
    val Re         = 625.0
    val lx         = 2.0
    val ly         = 2.0

    val units = new UnitsConverter(D2Q9, Re, physVel,lbVel,physLength,lbLength,lx,ly)
    
    val lattice = new Lattice2D( D2Q9, units.nX, units.nY,new BGKdynamics(D2Q9,units.omega) )
    
    iniGeometry(lattice,units)
//     val disp = Image( lattice.map( C => Math.sqrt(Arrays.normSqr(C.u)) ) ).display

    val maxT = 1000
    val logT = 10
    
    for (o <- 0 to 10) {

    val begin = Timer.go  
    
    for (iT <- 0 until maxT) {
      lattice.collideAndStream
//       if (iT % logT == 0) {
//         actor {
//             disp.update( lattice.map( C => Math.sqrt(Arrays.normSqr(C.u)) ) )
//         }
//       }
    }
    
    val end = Timer.stop
    val msups = 1.0 * maxT * (units.nX*units.nY)/ (end-begin) / 1.0e3
    
    
    println("MSUPS = " + msups )
    println("Total Time = " + (end-begin)/1000.0 )
    
    System.gc
    System.gc
		}
  }
}
