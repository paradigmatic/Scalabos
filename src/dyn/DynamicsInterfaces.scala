package lb.dyn

import lb.select._
import lb.dataProcessors2D._

object dynInterfaces {
  
  def defineDynamics(lattice:Lattice2D, domain:Region, dynamics:Dynamics) {
    lattice.select(domain).foreach(_.defineDynamics(dynamics))
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
