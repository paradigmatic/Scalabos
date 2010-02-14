package lb.util

import Math._


object Arrays {
  def copyArray[A]( ary: Array[A] ) = {
		// return the copy of an array
    val copy = new Array[A]( ary.length )
    Array.copy( ary, 0, copy, 0, ary.length )
    copy
  }

  def copyArrayDouble( ary: Array[Double] ) = {
    // return the copy of an array
    val copy = new Array[Double]( ary.length )
    Array.copy( ary, 0, copy, 0, ary.length )
    copy
  }

  def swap( i:Int, j:Int, f:Array[Double]) : Unit = {
		// swaps the elements i with element j in a array f.
    val tmp = f(i)
    f(i) = f(j)
    f(j) = tmp
  }
  
  def diff(u:Array[Double],v:Array[Double]) : Array[Double] = {
    val res = new Array[Double](u.length)
    for (iD <- 0 until u.length) res(iD) = u(iD) - v(iD)
    res
  }
  
  def dot(u:Array[Double], v:Array[Double]) : Double = {
		// scalar product TODO : check size
    var res:Double = u(0)*v(0)
    var iD = 1
    while ( iD < u.length ) {
      res += u(iD)*v(iD) 
      iD += 1
    }
    res
  }
  
  def dot(u:Array[Int], v:Array[Double]) : Double = {
		// scalar product TODO : check size
    var res:Double = u(0)*v(0)
    var iD = 1
    while ( iD < u.length ) {
      res += u(iD)*v(iD) 
      iD += 1
    }
    res
  }
  
  def normSqr(u:Array[Double]) : Double = { dot(u,u) } // norm squared
  
  def norm(u:Array[Double]) : Double = { sqrt(normSqr(u)) } // norm
  
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
