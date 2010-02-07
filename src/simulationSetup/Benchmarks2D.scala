package lb.simSetup

import lb.units._
import lb.util._

class Convergence(val minLength:Int, val epsilon:Double) {
	private var values = List[Double]()
	private var iter = 0
	
	private var mean = 0.0
	private var stdDev = 0.0
	
	def apply(value:Double) = { 
		values = value :: values 
		iter += 1
		if (values.length > minLength) {
			values = values.init
			val mean = Math.abs(values.reduceLeft(_+_)/values.length)
			val stdDev = Math.sqrt(values.map( x => Doubles.sqr(x-mean) ).reduceLeft(_+_))/values.length
			if (iter % minLength == 0) println("Avg = "+mean+", std dev = "+stdDev+", std dev / avg = "+stdDev/mean)
			if (stdDev / mean < epsilon) true
			else false
		}
		else false
	}
}

class TaylorGreen2D(val units:UnitsConverter, val m:Int, val n:Int) {
  
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
  
  def deviatoricStress(iX:Int, iY:Int) = new Array[Double](units.D.n)
}

class Poiseuille2D(val units:UnitsConverter, val dir:Int) {
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
  
  def deviatoricStress(iX:Int, iY:Int) = new Array[Double](units.D.n)
}

class Dipole(val omega_e:Double, val r0:Double, val x1:Double, val x2:Double, val y1:Double, val y2:Double, val units:UnitsConverter) {
  def r0sqr() = Doubles.sqr(r0)
      
  def r1sqr(x:Double, y:Double) = Doubles.sqr(x-x1) + Doubles.sqr(y-y1)
  
  def r2sqr(x:Double, y:Double) = Doubles.sqr(x-x2) + Doubles.sqr(y-y2)

  def density(iX:Int, iY:Int) = 1.0
      
  def velocity(iX:Int,iY:Int) : Array[Double] = {
    val x = iX*units.deltaX()-1.0
    val y = iY*units.deltaX()-1.0
    val u = new Array[Double](units.D.d)
    u(0) = -0.5 * omega_e*(y-y1) * Math.exp(-r1sqr(x,y)/r0sqr()) + 0.5 * omega_e*(y-y2) * Math.exp(-r2sqr(x,y)/r0sqr())
    u(1) = +0.5 * omega_e*(x-x1) * Math.exp(-r1sqr(x,y)/r0sqr()) - 0.5 * omega_e*(x-x2) * Math.exp(-r2sqr(x,y)/r0sqr())
    u.map( _*units.lbVel)
  }

  def deviatoricStress(iX:Int,iY:Int) : Array[Double] = {
    val x = iX*units.deltaX()-1.0
    val y = iY*units.deltaX()-1.0
            
    val dx_ux = .5*omega_e*(y-y1)*(2*x-2*x1)*Math.exp(-r1sqr(x,y)/r0sqr())/r0sqr()-.5*omega_e*(y-y2)*(2*x-2*x2)*Math.exp(-r2sqr(x,y)/r0sqr())/r0sqr()
    val dy_ux = -.5*omega_e*Math.exp(-r1sqr(x,y)/r0sqr())+.5*omega_e*(y-y1)*(2*y-2*y1)*Math.exp(-r1sqr(x,y)/r0sqr())/r0sqr()+.5*omega_e*Math.exp(-r2sqr(x,y)/r0sqr())-.5*omega_e*(y-y2)*(2*y-2*y2)*Math.exp(-r2sqr(x,y)/r0sqr())/r0sqr()
    val dx_uy = .5*omega_e*Math.exp(-r1sqr(x,y)/r0sqr())-.5*omega_e*(x-x1)*(2*x-2*x1)*Math.exp(-r1sqr(x,y)/r0sqr())/r0sqr()-.5*omega_e*Math.exp(-r2sqr(x,y)/r0sqr())+.5*omega_e*(x-x2)*(2*x-2*x2)*Math.exp(-r2sqr(x,y)/r0sqr())/r0sqr()
    val dy_uy = -.5*omega_e*(x-x1)*(2*y-2*y1)*Math.exp(-r1sqr(x,y)/r0sqr())/r0sqr()+.5*omega_e*(x-x2)*(2*y-2*y2)*Math.exp(-r2sqr(x,y)/r0sqr())/r0sqr()

    val sToPi = - density(iX, iY) / (3.0 * units.omega())

    val piNeq = new Array[Double](units.D.n)
    piNeq(0) = (2 * dx_ux * units.deltaT() * sToPi)      
    piNeq(1) = ((dx_uy + dy_ux) * units.deltaT() * sToPi)
    piNeq(2) = (2 * dy_uy * units.deltaT() * sToPi)       
    piNeq
  }
}