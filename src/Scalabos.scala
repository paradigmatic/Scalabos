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

  def main( args: Array[String] ) : Unit = {
    println("The first Scala lattice Boltzmann Solver (Scalabos) code EVER!!!")
    val physLength = 1.0
    val lbLength   = 20
    val physVel    = 1.0
    val lbVel      = 0.01
    val Re         = 1.0
    val lx         = 1.0
    val ly         = 1.0
    
    val units = new UnitsConverter(D2Q9, Re, physVel,lbVel,physLength,lbLength,lx,ly)
    
    val lattice = new Lattice2D( D2Q9, units.nX, units.nY,new BGKdynamics(D2Q9,units.omega) )
//    Image( lattice.map( _.rho ) ).display
    
//     val ini = new TaylorGreen2D(units,1,1)
    val ini = new Poiseuille2D(units,0)

    dynInterfaces.addVelocityBoundary(lattice,WestWall,new RegularizedVelocityBoundaryCondition(D2Q9, 0, -1))
    dynInterfaces.addVelocityBoundary(lattice,EastWall,new RegularizedVelocityBoundaryCondition(D2Q9, 0, +1))
    
    val applyBoundaryVelocity = SimSetup.defineVelocity(D2Q9,ini.velocity)
    lattice.select(WholeDomain).foreach(applyBoundaryVelocity)

    dynInterfaces.defineDynamics(lattice, NorthWall, new BounceBack(D2Q9))
    dynInterfaces.defineDynamics(lattice, SouthWall, new BounceBack(D2Q9))
    
    

    val applyInitialSetup = SimSetup.iniAtEquilibrium(D2Q9,ini.density,ini.velocity)
    lattice.select(WholeDomain).foreach(applyInitialSetup)
//     Image( lattice.map( _.rho ) ).display
//     Image( lattice.map( C => Arrays.normSqr(C.u) ) ).display

    val maxT = 1000
    val logT = 10

//     for( o <- 0 until 10 ) {
      val begin = Timer.go  
      
      for (iT <- 0 until maxT) { 
//         if (iT % logT == 0) println("This iteration is for you baby "+iT)
        if (iT % logT == 0) Arrays.dump( "vel"+iT+".dat", lattice.map( C => Arrays.normSqr(C.u) ) )
//         println(iT*units.deltaT + " " + Averages.energy(lattice, WholeDomain) + " " + Averages.density(lattice, WholeDomain))
        lattice.collideAndStream
      }
      
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
