package lb.util

import Math._

class Box2D(val x0:Int, val x1:Int, val y0:Int, val y1:Int) {
	def resize(size:Int) = new Box2D(x0-size,x1+size,y0-size,y1+size)
	
	def shiftX(offset:Int) = new Box2D(x0+offset,x1+offset,y0,y1)
	def shiftY(offset:Int) = new Box2D(x0,x1,y0+offset,y1+offset)
}

class Dot2D(val x:Int, val y:Int) {
	def shiftX(offset:Int) = new Dot2D(x+offset,y)
	def shiftY(offset:Int) = new Dot2D(x,y+offset)
	
	def shift(off:Dot2D) = new Dot2D(x+off.x,y+off.y)
}

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

import lb.desc._

object Indexes {
  def subIndex[T <: Descriptor](D:T, dir:Int, orient:Int) : Array[Int] =  {
  //determines the indices of the dir component of velocities pointing in orient direction
		Array.range(0, D.q).filter{iVel => D.c(iVel)(dir) == orient}
  }
  
  def extractSubArray[T](origArray:Array[T], subInd:Array[Int]) : Array[T] = {
		// Given an array of indices, returns an array containing the components
		// corresponding to the indices of an original array.
		val subArray = new Array[T](subInd.length)
		for (iPop <- 0 until subInd.length) subArray(iPop) = origArray(subInd(iPop))
		subArray
	}

  def extractSubArray(origArray:Array[Double], subInd:Array[Int]) : Array[Double] = {
		// Given an array of indices, returns an array containing the components
		// corresponding to the indices of an original array.
		val subArray = new Array[Double](subInd.length)
                var iPop = 0
                while( iPop < subInd.length ) {
                  subArray(iPop) = origArray(subInd(iPop))
                  iPop += 1
                }
		subArray
	}
  
  // Indices numerotation for tensors
  object dim2 {
    val xx = 0
    val xy = 1
    val yy = 2
  }
}

object Doubles {
  def sqr( u:Double ) = {
    u * u
  }
}

object Fd {
  def fwdDiff(u_1:Double,u:Double,orient:Int) = orient*(u_1 - u)
}
