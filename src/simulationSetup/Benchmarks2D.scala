package lb.simSetup

import lb.units._
import lb.util._

class TaylorGreen2D[T <: Descriptor](val units:UnitsConverter[T], val m:Int, val n:Int) {
  
  def density(iX:Int, iY:Int) = {
    lazy val pi = 4.0 * Math.atan(1.0)
    val x = iX.toDouble / (units.nX-1).toDouble
    val y = iY.toDouble / (units.nY-1).toDouble
    
    val pressure = -Doubles.sqr(pi* units.deltaT / units.deltaX) *(n*n*Math.cos(4.0*pi*m*x)+m*m*Math.cos(4.0*pi*n*y))
    1.0+pressure * units.D.invCs2
  }
  
  def velocity(iX:Int, iY:Int) : Array[Double] = {
    lazy val pi = 4.0 * Math.atan(1.0)
    val x = iX.toDouble / (units.nX-1).toDouble
    val y = iY.toDouble / (units.nY-1).toDouble
    
    val u = new Array[Double](2)
    
    u(0) = units.lbVel * (- n * 2.0 * pi * Math.sin(n * y * 2.0 * pi) * Math.cos(m * x * 2.0 * pi))
    u(1) = units.lbVel * (  m * 2.0 * pi * Math.sin(m * x * 2.0 * pi) * Math.cos(n * y * 2.0 * pi))
    
    u
  } 
}

class Poiseuille2D[T <: Descriptor](val units:UnitsConverter[T], val dir:Int) {
  def density(iX:Int, iY:Int) = {
    lazy val lx = units.nX-1
    lazy val ly = units.nY-1
    
    if (dir == 0) 1.0+units.D.invCs2*(8.0 * units.lbNu*units.lbVel / (ly*ly) * (lx/2.0 - iX) )
    else 1.0+units.D.invCs2*(8.0 * units.lbNu*units.lbVel / (lx*lx) * (ly/2.0 - iY) )
  }
  
  def velocity(iX:Int, iY:Int) : Array[Double] = {
    var pos = 0.0
    if (dir == 0) pos = iY.toDouble / units.lbLength
    else pos = iX.toDouble / units.lbLength
    
    val u = new Array[Double](units.D.d)
    
    u(dir) = 4.0*units.lbVel * (pos-pos*pos)
    
    u
  }
}