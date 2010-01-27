package lb.util

import Math._

object Arrays {
  def copyArray[A]( ary: Array[A] ) = {
		// return the copy of an array
    val copy = new Array[A]( ary.size )
    Array.copy( ary, 0, copy, 0, ary.size )
    copy
  }
  
  def swap( i:Int, j:Int, f:Array[Double]) : Unit = {
		// swaps the elements i with element j in a array f.
    val tmp = f(i)
    f(i) = f(j)
    f(j) = tmp
  }
  
  def dot(u:Array[Double], v:Array[Double]) : Double = {
		// scalar product TODO : check size
    var res:Double = u(0)*v(0)
    for( iD <- 1 until u.length) { res += u(iD)*v(iD) }
    res
  }
  
  def dot(u:Array[Int], v:Array[Double]) : Double = {
		// scalar product TODO : check size
    var res:Double = u(0)*v(0)
    for( iD <- 1 until u.length) { res += u(iD)*v(iD) }
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

object Indexes {
  def subIndex[T <: Descriptor](D:T, dir:Int, orient:Int) : Array[Int] =  {
  //determines the indices of the dir component of velocities pointing in orient direction
		Array.range(0, D.q).filter{iVel => D.c(iVel)(dir) == orient}
  }
  
  def extractSubArray[T](origArray:Array[T], subInd:Array[Int]) : Array[T] = {
		// Given an array of indices, returns an array containing the components
		// corresponding to the indices of an original array.
		val subArray = new Array[T](subInd.size)
		for (iPop <- 0 until subInd.size) subArray(iPop) = origArray(subInd(iPop))
		subArray
	}
}

object Doubles {
  def sqr( u:Double ) = {
    u * u
  }
}
