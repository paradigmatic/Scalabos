package lb.util

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
