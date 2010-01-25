package lb.util

import Math._

object Util {
  
  class Box2D(val x0:Int,val x1:Int, val y0:Int, val y1:Int) {}

  def copyArray[A]( ary: Array[A] ) = {
    val copy = new Array[A]( ary.size )
    Array.copy( ary, 0, copy, 0, ary.size )
    copy
  }
  
  def sqr( u:Double ) = {
    u * u
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
  
  def dot(u:Array[Int], v:Array[Double]) : Double = {
    var res:Double = u(0)*v(0)
    for( iD <- 1 until u.size) { res += u(iD)*v(iD) }
    res
  }
  
  def normSqr(u:Array[Double]) : Double = { dot(u,u) }
  
  def norm(u:Array[Double]) : Double = { sqrt(normSqr(u)) }

  def dump[T](filename: String, matrix: Array[Array[T]] ) {
    import java.io._
    val out = new PrintWriter( new File( filename ) )
    matrix foreach { 
      row =>
        out println row.mkString(" ") 
    }
    out close
    
  }
    
}
