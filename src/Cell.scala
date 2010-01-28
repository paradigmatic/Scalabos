package lb

import lb.dyn._
import lb.util.Arrays._

class Cell[T <: Descriptor]( var dyn: Dynamics[T] ) {
	private lazy val half = D.q/2
	private lazy val oneToHalfRange = (1 to half).toList  
	
  private var f = copyArrayDouble( D.t )
  
  def apply(iPop:Int) : Double = f(iPop)

  def collide() = dyn(f)
  
  def D() : Descriptor = dyn.D
  
  def defineDynamics( dynamics:Dynamics[T] ) = {
    dyn = dynamics
  }
  
  def revert(): Unit = {
    for (iPop <- oneToHalfRange) { swap(iPop,iPop+half,f) }
  }

  def rho() = dyn.rho(f)

  def u() = dyn.u(f,rho)
  
  def update(iPop:Int, rhs:Double) = { f(iPop) = rhs }

  override def toString() = "Cell("+ dyn+", rho=" + rho + ", u=" + u.toString +")"

}
