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

abstract class BGKdynamics extends IncompressibleDynamics {
  
  def apply( f: Array[Double] ) {}
}
