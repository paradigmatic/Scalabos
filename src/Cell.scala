package lb

import lb.dyn._

abstract class Cell( var dyn: Dynamics ) extends Descriptor {

  private val f = new Array[Double](Q) //FIXME: init with correct size

  def collide() = dyn(f)

  def rho() = dyn.rho(f)

  def u() = dyn.u(f,rho)

}
