package lb.dyn

import lb.util._

abstract class Dynamics(val D: Descriptor) {
  
  def apply( f: Array[Double] ): Unit // The apply methods does the collision
  
  def copy() : Dynamics // The returns a new instance of the class
  
  def defineVelocity(u:Array[Double]) : Unit = {}
  
  def equilibrium(iPop:Int,rho:Double,u:Array[Double],uSqr:Double) : Double
  def fOne(iPop:Int,piNeq:Array[Double]) : Double // The off-equilibrium contribution to the distribution function
  
  def regularize(iPop:Int,rho:Double,u:Array[Double],piNeq:Array[Double]) : Double = {
    val uSqr = Arrays.normSqr(u)
    // f = feq+fone
    equilibrium(iPop,rho,u,uSqr)+fOne(iPop,piNeq)
  }
  
  def rho( f: Array[Double]): Double
  def u( f: Array[Double], rho: Double ): Array[Double]
  def deviatoricStress(f:Array[Double], rho:Double, u:Array[Double]) : Array[Double]
  
  def omega():Double
}

class NoDynamics(D:Descriptor) extends Dynamics(D) {

  def apply( f: Array[Double] ) {}
  
  def copy() = new NoDynamics(D)
  
  def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double) = D.t(iPop)
  def fOne(iPop:Int,piNeq:Array[Double]) = 0.0
  
  def rho( f: Array[Double]) = 1.0
  def u( f: Array[Double], rho: Double ) = new Array[Double](D.d)
  def deviatoricStress(f:Array[Double], rho:Double, u:Array[Double])  = new Array[Double](D.n)
  
  def omega() = 0.0
  
  override lazy val toString = "No Dynamics"
}

class BounceBack(D:Descriptor) extends Dynamics(D) {
  
  def apply( f: Array[Double] ) {  
    lazy val half = D.q/2
    for (iPop <- 1 until half+1) Arrays.swap(iPop,iPop+half,f)
  }
  
  def copy() = new BounceBack(D)
  
  def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double) = D.t(iPop)
  def fOne(iPop:Int,piNeq:Array[Double]) = 0.0
  
  def rho( f: Array[Double]) = 1.0
  def u( f: Array[Double], rho: Double ) = new Array[Double](D.d)
  def deviatoricStress(f:Array[Double], rho:Double, u:Array[Double])  = new Array[Double](D.n)
  
  def omega() = 0.0
  
  override lazy val toString = "BounceBack"
}

abstract class IncompressiBleDynamics(D:Descriptor) extends Dynamics(D) {

  private lazy val myPopIndices = (1 until D.q).toList

  def rho( f: Array[Double]): Double =  f.reduceLeft(_+_)
  def u( f: Array[Double], rho: Double ) = {
    val vel = new Array[Double](D.d)
    for( iPop <- myPopIndices; iD <- D.dimIndices) vel(iD) += D.c(iPop)(iD)*f(iPop)
    vel.map(_ / rho)
  }
  
  def deviatoricStress(f:Array[Double], rho:Double, u:Array[Double]) = {
    var iPi = 0
    val piNeq = new Array[Double](D.n)
    for (iA <- D.dimIndices) { 
      val iDiAg = iPi
      for (iB <- iA until D.d) { 
        for (iPop <- myPopIndices) {
          piNeq(iPi) += D.c(iPop)(iA)*D.c(iPop)(iB)*f(iPop)
        }
        piNeq(iPi) -= rho*u(iA)*u(iB)
        iPi += 1
      }
      piNeq(iDiAg) -= D.cs2 * rho
    }
    piNeq
  }
}

class BGKdynamics(D:Descriptor, om:Double) extends IncompressiBleDynamics(D) {
  
  def copy() = new BGKdynamics(D, omega)
  
  def fOne(iPop:Int,piNeq:Array[Double]) = {
    var fNeq = 0.5*D.t(iPop)*Doubles.sqr(D.invCs2)
    var q_pi = 0.0
    var iPi = 0
    for (iA <- D.dimIndices; iB <- iA until D.d) {
      if (iA == iB) {
        q_pi += D.c(iPop)(iA)*D.c(iPop)(iB)*piNeq(iPi)
        q_pi -= D.cs2*piNeq(iPi)
      }
      else q_pi += 2.0*D.c(iPop)(iA)*D.c(iPop)(iB)*piNeq(iPi)
      iPi += 1
    }
                     
    fNeq *= q_pi
    fNeq
  }
  
  def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double) : Double = {
    var c_u = Arrays.dot(D.c(iPop),u)
    c_u *= D.invCs2
    
    D.t(iPop)*rho*(1.0 + c_u + 0.5 * (c_u*c_u - D.invCs2*uSqr ))
  }
  
  def omega() = om
  
  def apply( f: Array[Double] ) = {
    val dens:Double = rho(f)
    val vel:Array[Double] = u(f,dens)
    val velSqr = Arrays.normSqr(vel)
    
    for (iPop <- D.popIndices) { 
      f(iPop) *= (1.0 - omega)
      f(iPop) += omega*equilibrium(iPop,dens,vel,velSqr)
    }
  }

  override lazy val toString = "BGK("+D+", omega="+omega+")"
}
