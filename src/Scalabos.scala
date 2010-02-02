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
    
    val ww = Rectangle( 0,          0,          1,          units.nY-2 )
    val ew = Rectangle( units.nX-1, units.nX-1, 1,          units.nY-2 )
    val sw = Rectangle( 1,          units.nX-2, 0,          0 )
    val nw = Rectangle( 1,          units.nX-2, units.nY-1, units.nY-1 )
    
    dynInterfaces.addVelocityBoundary(lattice,ww,new RegularizedVelocityBoundaryCondition(D2Q9, 0, -1))
    dynInterfaces.addVelocityBoundary(lattice,ew,new RegularizedVelocityBoundaryCondition(D2Q9, 0, +1))
    dynInterfaces.addVelocityBoundary(lattice,sw,new RegularizedVelocityBoundaryCondition(D2Q9, 1, -1))
    dynInterfaces.addVelocityBoundary(lattice,nw,new RegularizedVelocityBoundaryCondition(D2Q9, 1, +1))
    
    val nwc = Rectangle( 0,          0,          units.nY-1, units.nY-1 )
    val swc = Rectangle( 0,          0,          0,          0 )
    val nec = Rectangle( units.nX-1, units.nX-1, units.nY-1, units.nY-1 )
    val sec = Rectangle( units.nX-1, units.nX-1, 0,          0 )
    
    dataProcessorsInterfaces.addDataProcessor(lattice,new CornerBoundaryConditionProcessor2D(lattice,nwc,-1,+1))
    dataProcessorsInterfaces.addDataProcessor(lattice,new CornerBoundaryConditionProcessor2D(lattice,swc,-1,-1))
    dataProcessorsInterfaces.addDataProcessor(lattice,new CornerBoundaryConditionProcessor2D(lattice,nec,+1,+1))
    dataProcessorsInterfaces.addDataProcessor(lattice,new CornerBoundaryConditionProcessor2D(lattice,sec,+1,-1))
    
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

    val maxT = 1000
    val logT = 1

     for( o <- 0 until 10 ) {
      val begin = Timer.go  
      
      for (iT <- 0 until maxT) { 
//         if (iT % logT == 0) println("This iteration is for you baby "+iT)
        //if (iT % logT == 0) Arrays.dump( "vel"+iT+".dat", lattice.map( C => Math.sqrt(Arrays.normSqr(C.u)) ) )
//         println(iT*units.deltaT + " " + Averages.energy(lattice, WholeDomain) + " " + Averages.density(lattice, WholeDomain))
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
     }
  }
  //Image( lattice.map( _.rho ) ).display
}
