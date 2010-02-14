package lb.dyn

import lb.util._
import lb.dyn.helpers._

class BGKdynamics(D:Descriptor, om:Double) extends IncompressiBleDynamics(D) {
  
  def copy() = new BGKdynamics(D, omega)
  
  def fOne(iPop:Int,piNeq:Array[Double]) = {
    var fNeq = 0.5*D.t(iPop)*Doubles.sqr(D.invCs2)
    var q_pi = 0.0
    var iPi = 0
    var iA = 0
    while( iA < D.d ) {
      var iB = iA
      while( iB < D.d ) {
        if (iA == iB) {
          q_pi += D.c(iPop)(iA)*D.c(iPop)(iB)*piNeq(iPi)
          q_pi -= D.cs2*piNeq(iPi)
        } else { 
          q_pi += 2.0*D.c(iPop)(iA)*D.c(iPop)(iB)*piNeq(iPi)
        }
        iPi += 1
        iB += 1
      }
      iA += 1
    }
                     
    fNeq *= q_pi
    fNeq
  }
  
  def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double) : Double = {
    
    lbHelpers.equilibrium(D,iPop,rho,u,uSqr)
  }
  
  def omega() = om
  
  def apply( f: Array[Double] ) = {
    val dens:Double = rho(f)
    val vel:Array[Double] = u(f,dens)
    
    lbHelpers.bgkCollision(D,f,dens,vel,omega)
    
//     val velSqr = Arrays.normSqr(vel)
//     
//     for (iPop <- D.popIndices) { 
//       f(iPop) *= (1.0 - omega)
//       f(iPop) += omega*equilibrium(iPop,dens,vel,velSqr)
//     }
  }

  override lazy val toString = "BGK("+D+", omega="+omega+")"
}
