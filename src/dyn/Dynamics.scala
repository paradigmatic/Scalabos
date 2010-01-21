package lb.dyn

trait Dynamics {
  
  def apply( f: Array[Double] ): Unit
  def revert( f: Array[Double] ): Unit
  def rho( f: Array[Double]): Double
  def u( f: Array[Double], rho: Double ): Array[Double]

}

object NoDynamics extends Dynamics {

  def apply( f: Array[Double] ) {}
  def revert( f: Array[Double] ) {}
  def rho( f: Array[Double]) = 1.0
  def u( f: Array[Double], rho: Double ) = Array( 0.0, 0.0 )

}
