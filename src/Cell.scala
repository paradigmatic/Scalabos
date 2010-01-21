package lb

import lb.dyn._
import lb.util.Util._

abstract class Cell( var dyn: Dynamics ) extends Descriptor {

  private var f = copyArray( T )

  def collide() = dyn(f)
  
  def revert(): Unit = {
    val half = Q/2
    for (iPop <- 1 until half+1) { swap(iPop,iPop+half,f) }
  }

  def rho() = dyn.rho(f)

  def u() = dyn.u(f,rho)

}
