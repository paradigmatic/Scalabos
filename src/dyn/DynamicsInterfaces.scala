package lb.dyn

import lb.select._

object dynInterfaces {
  
  def defineDynamics[T <: Descriptor](lattice:Lattice2D[T], domain:Region, dynamics:Dynamics[T]) {
    lattice.select(domain).foreach(_.defineDynamics(dynamics))
  }
  
  def addVelocityBoundary[T <: Descriptor](lattice:Lattice2D[T],domain:Region,bcDyn:DirichletVelocityDynamics[T]) {
    lattice.select(domain).foreach(C => { val baseDyn = C.dyn; bcDyn.defineBaseDynamics(baseDyn); C.defineDynamics(bcDyn)})
  }

}