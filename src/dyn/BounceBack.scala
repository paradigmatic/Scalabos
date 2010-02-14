package lb.dyn
import lb.util._

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
