package lb.dyn

import lb.select._

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

}
