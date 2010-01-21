package lb.dyn

import lb.util.Util._

trait Dynamics extends Descriptor {
  
  def apply( f: Array[Double] ): Unit
  def revert( f: Array[Double] ): Unit = {
    val half = Q/2
    for (iPop <- 1 until half+1) { swap(iPop,iPop+half,f) }
  }
  def rho( f: Array[Double]): Double
  def u( f: Array[Double], rho: Double ): Array[Double]

}

abstract class NoDynamics extends Dynamics {

  def apply( f: Array[Double] ) {}
  def rho( f: Array[Double]) = 1.0
  def u( f: Array[Double], rho: Double ) = new Array[Double](D)

}

trait IncompressibleDynamics extends Dynamics {

  def rho( f: Array[Double]): Double =  f.reduceLeft(_+_)
  def u( f: Array[Double], rho: Double ) = {
    val vel = new Array[Double](D)
    for( iPop <- 1 until Q; iD <- 0 until D) vel(iD) += C(iPop)(iD)*f(iPop)
    vel.map(_ / rho)
  }
}

abstract class BGKdynamics(om:Double) extends IncompressibleDynamics {
  
  private var omega = om // the relaxation frequency
  
  def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double) : Double = {
    var c_u = 0.0
    for (iD <- 0 until D) { c_u += C(iPop)(iD) * u(iD) }
    c_u *= invCs2
    
    T(iPop)*rho*(1.0 + c_u + 0.5 * (c_u*c_u - invCs2*uSqr ))
  }
  
  def apply( f: Array[Double] ) = {
    val dens:Double = rho(f)
    val vel:Array[Double] = u(f,dens)
    val velSqr = normSqr(vel)
    
    for (iPop <- 0 until Q) { 
      f(iPop) *= (1.0 - omega)
      f(iPop) += omega*equilibrium(iPop,dens,vel,velSqr)
    }
  }
}
