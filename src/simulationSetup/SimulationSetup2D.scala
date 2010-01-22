package lb.simSetup

import lb.util.Util._

object SimSetup {
  def iniAtEquilibrium[T <: Descriptor](fRho : (Int, Int) => Double, fU : (Int,Int) => Array[Double])
                                       (lattice:Lattice2D[T], domain:Box2D) {
    for (iX <- domain.x0 to domain.x1; iY <- domain.y0 to domain.y1; iPop <- 0 until lattice.D.q) {
      val rho  = fRho(iX,iY)
      val u    = fU(iX,iY)
      val uSqr = dot(u,u)
      lattice(iX,iY)(iPop) = lattice(iX,iY).dyn.equilibrium(iPop,rho,u,uSqr)
    }
  }
}


