package lb.util

import Math._

object Util {

  def copyArray[A]( ary: Array[A] ) = {
    val copy = new Array[A]( ary.size )
    Array.copy( ary, 0, copy, 0, ary.size )
    copy
  }
  
  def swap( i:Int, j:Int, f:Array[Double]) : Unit = {
    val tmp = f(i)
    f(i) = f(j)
    f(j) = tmp
  }
  
  def dot(u:Array[Double], v:Array[Double]) : Double = {
    var res:Double = u(0)*v(0)
    for( iD <- 1 until u.size) { res += u(iD)*v(iD) }
    res
  }
  
  def normSqr(u:Array[Double]) : Double = { dot(u,u) }
  
  def norm(u:Array[Double]) : Double = { sqrt(normSqr(u)) }
    
}
