package lb.util

object Util {

  def copyArray[A]( ary: Array[A] ) = {
    val copy = new Array[A]( ary.size )
    Array.copy( ary, 0, copy, 0, ary.size )
    copy
  }


}
