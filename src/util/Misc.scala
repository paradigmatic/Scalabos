package lb.util

object Doubles {
  def sqr( u:Double ) = {
    u * u
  }
}

object Fd {
  def fwdDiff(u_1:Double,u:Double,orient:Int) = orient*(u_1 - u)
}
