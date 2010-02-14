package lb.dyn

import lb.util._
import lb.dyn.helpers._

abstract class Dynamics(val D: Descriptor) {
  
  def apply( f: Array[Double] ): Unit // The apply methods does the collision
  
  def copy() : Dynamics // The returns a new instance of the class
  
  def defineDensity(rho:Double) :Unit = {}
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



abstract class IncompressiBleDynamics(D:Descriptor) extends Dynamics(D) {

  private lazy val myPopIndices = (1 until D.q).toList

	def rho( f: Array[Double]): Double =  lbHelpers.computeRho(D,f)
//   def rho( f: Array[Double]): Double =  f.reduceLeft(_+_)
  def u( f: Array[Double], rho: Double ) = {
      lbHelpers.computeU(D,f,rho)
//     val vel = new Array[Double](D.d)
//     for( iPop <- myPopIndices; iD <- D.dimIndices) vel(iD) += D.c(iPop)(iD)*f(iPop)
//     vel.map(_ / rho)
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

abstract class CompositeDynamics(D:Descriptor) extends Dynamics(D) {
  var baseDyn:Dynamics = new NoDynamics(D)
  
  def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double): Double = baseDyn.equilibrium(iPop,rho,u,uSqr)
  def fOne(iPop:Int, piNeq:Array[Double]): Double = baseDyn.fOne(iPop,piNeq)
  
  override def regularize(iPop:Int,rho:Double, u:Array[Double], piNeq:Array[Double]) : Double = baseDyn.regularize(iPop,rho,u,piNeq)
  
  def defineBaseDynamics(dyn:Dynamics) = { baseDyn = dyn }
  
  def completePopulations(f:Array[Double]) : Unit
  
  def omega() = baseDyn.omega
  
  override def apply(f:Array[Double]) = {
    completePopulations(f)
    baseDyn(f)
  }
}
