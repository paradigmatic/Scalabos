package lb.dyn

import lb.util._

abstract class Dynamics[T <: Descriptor](val D:T) {
  
  def apply( f: Array[Double] ): Unit
  
  def defineVelocity(u:Array[Double]) {}
  
  def equilibrium(iPop:Int,rho:Double,u:Array[Double],uSqr:Double) : Double
  def fOne(iPop:Int,piNeq:Array[Double]) : Double
  
  def regularize(iPop:Int,rho:Double,u:Array[Double],piNeq:Array[Double]) : Double = {
    val uSqr = Arrays.normSqr(u)
    equilibrium(iPop,rho,u,uSqr)+fOne(iPop,piNeq)
  }
  
  def regularize(rho:Double,u:Array[Double],piNeq:Array[Double]) : Array[Double] = {
    val uSqr = Arrays.normSqr(u)
    val f = new Array[Double](D.q)
    for (iPop <- D.popIndices) f(iPop) = equilibrium(iPop,rho,u,uSqr)+fOne(iPop,piNeq)+fOne(iPop,piNeq)
    f
  }
  
  def rho( f: Array[Double]): Double
  def u( f: Array[Double], rho: Double ): Array[Double]
  def deviatoricStress(f:Array[Double], rho:Double, u:Array[Double]) : Array[Double]

}

class NoDynamics[T <: Descriptor](override val D:T) extends Dynamics(D) {

  def apply( f: Array[Double] ) {}
  
  def equilibrium(iPop:Int,rho:Double,u:Array[Double],uSqr:Double) = 0.0
  def fOne(iPop:Int,piNeq:Array[Double]) = 0.0
  
  def rho( f: Array[Double]) = 1.0
  def u( f: Array[Double], rho: Double ) = new Array[Double](D.d)
  def deviatoricStress(f:Array[Double], rho:Double, u:Array[Double])  = new Array[Double](D.n)

}

class BounceBack[T <: Descriptor](override val D:T) extends Dynamics(D) {
  
  def apply( f: Array[Double] ) {  
    lazy val half = D.q/2
    for (iPop <- 1 until half+1) Arrays.swap(iPop,iPop+half,f)
  }
  
  def equilibrium(iPop:Int,rho:Double,u:Array[Double],uSqr:Double) = 0.0
  def fOne(iPop:Int,piNeq:Array[Double]) = 0.0
  
  def rho( f: Array[Double]) = 1.0
  def u( f: Array[Double], rho: Double ) = new Array[Double](D.d)
  def deviatoricStress(f:Array[Double], rho:Double, u:Array[Double])  = new Array[Double](D.n)
}

abstract class IncompressiBleDynamics[T <: Descriptor](override val D:T) extends Dynamics(D) {

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

class BGKdynamics[T <: Descriptor](override val D:T, var omega:Double) extends IncompressiBleDynamics(D) {
  
  def fOne(iPop:Int,piNeq:Array[Double]) = {
    var fNeq = 0.5*D.t(iPop)*Doubles.sqr(D.invCs2)
    var q_pi = 0.0
    var iPi = 0
    for (iA <- D.dimIndices; iB <- iA until D.d) {
      if (iA == iB) {
        q_pi += D.c(iPop)(iA)*D.c(iPop)(iB)*piNeq(iPi)
        q_pi -= D.cs2*piNeq(iPi)
      }
      else q_pi += 2*D.c(iPop)(iA)*D.c(iPop)(iB)*piNeq(iPi)
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
