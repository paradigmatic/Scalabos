package lb

// import lb._
import lb.dyn._
import lb.units._
import lb.simSetup._
import lb.util._
import lb.visual._
import lb.select._
import lb.dataProcessors2D._

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
    val ini = new Poiseuille2D(units,0)
    
    dynInterfaces.addVelocityBoundaryConditionOnBoundingBox(lattice)
    
    val applyBoundaryVelocity = SimSetup.defineVelocity(D2Q9,ini.velocity)
    lattice.select(WholeDomain).foreach(applyBoundaryVelocity)
    
    val applyInitialSetup = SimSetup.iniAtEquilibrium(D2Q9,ini.density,ini.velocity)
    lattice.select(WholeDomain).foreach(applyInitialSetup)
  }

  def main( args: Array[String] ) : Unit = {
    println("The first Scala lattice Boltzmann Solver (Scalabos) code EVER!!!")
    val physLength = 1.0
    val lbLength   = 50
    val physVel    = 1.0
    val lbVel      = 0.01
    val Re         = 1.0
    val lx         = 1.5
    val ly         = 1.0
    
    val units = new UnitsConverter(D2Q9, Re, physVel,lbVel,physLength,lbLength,lx,ly)
    
    val lattice = new Lattice2D( D2Q9, units.nX, units.nY,new BGKdynamics(D2Q9,units.omega) )
//    Image( lattice.map( _.rho ) ).display
    
    iniGeometry(lattice,units)
//     Image( lattice.map( _.rho ) ).display
//     Image( lattice.map( C => Math.sqrt(Arrays.normSqr(C.u)) ) ).display

    val maxT = 10000
    val logT = 100
    val poiseuille = new Poiseuille2D(units,0)

//     for( o <- 0 until 10 ) {
      val begin = Timer.go  
      
      for (iT <- 0 until maxT) { 
//         if (iT % logT == 0) println("This iteration is for you baby "+iT)
//         println(iT*units.deltaT + " " + Averages.energy(lattice, WholeDomain) + " " + Averages.density(lattice, WholeDomain))
        if (iT % logT == 0) {
//           Arrays.dump( "vel"+iT+".dat", lattice.map( C => Math.sqrt(Arrays.normSqr(C.u)) ) )
          println("L2-average error = "+Averages.velocityL2Error(lattice, poiseuille.velocity)/units.lbVel)
        }

        lattice.collideAndStream
      }
      
//       Image( lattice.map( C => Math.sqrt(Arrays.normSqr(C.u)) ) ).display
      
      val end = Timer.stop
      //Image( lattice.map( _.rho ) ).display
      val msups = 1.0 * maxT * (units.nX*units.nY)/ (end-begin) / 1.0e3
      
      
      println("MSUPS = " + msups )
      println("Total Time = " + (end-begin)/1000.0 )
      System.gc
      System.gc
//     }
  }
  //Image( lattice.map( _.rho ) ).display
}
