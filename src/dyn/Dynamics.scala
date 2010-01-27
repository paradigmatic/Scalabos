package lb.dyn

import lb.util.Arrays._

abstract class Dynamics[T <: Descriptor](val D:T) {
  
  def apply( f: Array[Double] ): Unit
  
  def equilibrium(iPop:Int,rho:Double,u:Array[Double],uSqr:Double) : Double
  
  def rho( f: Array[Double]): Double
  def u( f: Array[Double], rho: Double ): Array[Double]

}

class NoDynamics[T <: Descriptor](override val D:T) extends Dynamics(D) {

  def apply( f: Array[Double] ) {}
  
  def equilibrium(iPop:Int,rho:Double,u:Array[Double],uSqr:Double) = D.t(iPop)
  
  def rho( f: Array[Double]) = 1.0
  def u( f: Array[Double], rho: Double ) = new Array[Double](D.d)

}

abstract class IncompressibleDynamics[T <: Descriptor](override val D:T) extends Dynamics(D) {

  private lazy val myPopIndices = (1 until D.q).toList

  def rho( f: Array[Double]): Double =  f.reduceLeft(_+_)
  def u( f: Array[Double], rho: Double ) = {
    val vel = new Array[Double](D.d)
    for( iPop <- myPopIndices; iD <- D.dimIndices) vel(iD) += D.c(iPop)(iD)*f(iPop)
    vel.map(_ / rho)
  }
}

class BGKdynamics[T <: Descriptor](override val D:T, var omega:Double) extends IncompressibleDynamics(D) {
  
  def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double) : Double = {
    var c_u = dot(D.c(iPop),u)
    c_u *= D.invCs2
    
    D.t(iPop)*rho*(1.0 + c_u + 0.5 * (c_u*c_u - D.invCs2*uSqr ))
  }
  
  def apply( f: Array[Double] ) = {
    val dens:Double = rho(f)
    val vel:Array[Double] = u(f,dens)
    val velSqr = normSqr(vel)
    
    for (iPop <- D.popIndices) { 
      f(iPop) *= (1.0 - omega)
      f(iPop) += omega*equilibrium(iPop,dens,vel,velSqr)
    }
  }

  override lazy val toString = "BGK("+D+", omega="+omega+")"
}
