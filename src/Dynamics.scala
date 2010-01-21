package lb

trait Dynamics {
  
  def apply( f: Array[Double] ): Unit
  def revert( f: Array[Double] ): Unit
  def rho( f: Array[Double]): Double
  def u( f: Array[Double], rho: Double ): Array[Double]

}
