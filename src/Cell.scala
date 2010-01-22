package lb

import lb.dyn._
import lb.util.Util._

class Cell[T <: Descriptor]( var dyn: Dynamics[T] ) {

  private var f = copyArray( D.t )
  
  def D() : Descriptor = dyn.D
  
  def apply(iPop:Int) : Double = f(iPop)
  
  def update(iPop:Int, rhs:Double) = { f(iPop) = rhs }

  def collide() = dyn(f)
  
  def revert(): Unit = {
    val half = D.q/2
    for (iPop <- 1 until half+1) { swap(iPop,iPop+half,f) }
  }

  def rho() = dyn.rho(f)

  def u() = dyn.u(f,rho)

}
