package lb

class Cell( var dyn: Dynamics ) {

  private val f = new Array[Double](1) //FIXME: init with correct size

  def collide() = dyn(f)

  def rho() = dyn.rho(f)

  def u() = dyn.u(f,rho)

}
