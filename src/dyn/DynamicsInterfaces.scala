package lb.dyn

import lb.select._
import lb.dataProcessors2D._

object dynInterfaces {
  
  def defineDynamics(lattice:Lattice2D, domain:Region, dynamics:Dynamics) {
    lattice.select(domain).foreach(_.defineDynamics(dynamics))
  }
  
  def addVelocityBoundaryConditionOnBoundingBox(lattice:Lattice2D) {
    val ww = Rectangle( 0,            0,            1,            lattice.nY-2 )
    val ew = Rectangle( lattice.nX-1, lattice.nX-1, 1,            lattice.nY-2 )
    val sw = Rectangle( 1,            lattice.nX-2, 0,            0 )
    val nw = Rectangle( 1,            lattice.nX-2, lattice.nY-1, lattice.nY-1 )
    
    addVelocityBoundary(lattice,ww,new RegularizedVelocityBoundaryCondition(D2Q9, 0, -1))
    addVelocityBoundary(lattice,ew,new RegularizedVelocityBoundaryCondition(D2Q9, 0, +1))
    addVelocityBoundary(lattice,sw,new RegularizedVelocityBoundaryCondition(D2Q9, 1, -1))
    addVelocityBoundary(lattice,nw,new RegularizedVelocityBoundaryCondition(D2Q9, 1, +1))
    
    val nwc = Rectangle( 0,            0,            lattice.nY-1, lattice.nY-1 )
    val swc = Rectangle( 0,            0,            0,            0 )
    val nec = Rectangle( lattice.nX-1, lattice.nX-1, lattice.nY-1, lattice.nY-1 )
    val sec = Rectangle( lattice.nX-1, lattice.nX-1, 0,            0 )
    
    addExternalVelocityCornerBoundary(lattice,nwc,-1,+1)
    addExternalVelocityCornerBoundary(lattice,swc,-1,-1)
    addExternalVelocityCornerBoundary(lattice,nec,+1,+1)
    addExternalVelocityCornerBoundary(lattice,sec,+1,-1)
  }
  
  def addVelocityBoundary(lattice:Lattice2D,domain:Region,bcDyn:DirichletVelocityDynamics) {
    lattice.select(domain).foreach( C => {
      val baseDyn = C.dyn
      bcDyn.defineBaseDynamics(baseDyn)
      C.defineDynamics(bcDyn.copy)} )
  }
  
  def addExternalVelocityCornerBoundary(lattice:Lattice2D,domain:Region,xNormal:Int,yNormal:Int) {
    defineDynamics(lattice,domain,new ImposedDensityAndVelocityDynamics(lattice.D))
    val dataProc = new CornerBoundaryConditionProcessor2D(lattice,domain,xNormal,yNormal)
    dataProcessorsInterfaces.addDataProcessor(lattice,dataProc)
  }

}
