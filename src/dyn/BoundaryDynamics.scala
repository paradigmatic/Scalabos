package lb.dyn

import lb.util._

abstract class CompositeDynamics(D:Descriptor) extends Dynamics(D) {
	var baseDyn:Dynamics = new NoDynamics(D)
  
  def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double): Double = baseDyn.equilibrium(iPop,rho,u,uSqr)
  def fOne(iPop:Int, piNeq:Array[Double]): Double = baseDyn.fOne(iPop,piNeq)
  
  override def regularize(iPop:Int,rho:Double, u:Array[Double], piNeq:Array[Double]) : Double = baseDyn.regularize(iPop,rho,u,piNeq)
	
	def defineBaseDynamics(dyn:Dynamics) = { baseDyn = dyn }
	
	def completePopulations(f:Array[Double]) : Unit
  
  def omega() = baseDyn.omega
	
	override def apply(f:Array[Double]) = {
		completePopulations(f)
		baseDyn(f)
	}
}

class ImposedDensityAndVelocityDynamics(D:Descriptor) extends CompositeDynamics(D) {
  
  var rhoBC = 1.0
  var uBC = new Array[Double](D.d)
  
  def copy() = {
    val tmpDyn = new ImposedDensityAndVelocityDynamics(D)
    tmpDyn.baseDyn = baseDyn
    tmpDyn.uBC = uBC
    tmpDyn.rhoBC = rhoBC
    tmpDyn
  }
  
  override def defineVelocity(u:Array[Double]) = {uBC = u}
  override def defineDensity(rho:Double) = {rhoBC = rho}
  
  def completePopulations(f:Array[Double]) = {}
  
  def rho( f: Array[Double]) : Double = rhoBC
  def u( f: Array[Double], rho: Double ) = uBC
  def deviatoricStress(f:Array[Double], density:Double, vel:Array[Double]) = baseDyn.deviatoricStress(f,rhoBC,uBC)
}

abstract class DirichletVelocityDynamics(D:Descriptor, val dir:Int, val orient:Int) extends CompositeDynamics(D) {

	var uBC = new Array[Double](D.d)
	
	lazy val onWallIndices = Indexes.subIndex(D,dir,0)
	lazy val normalIndices = Indexes.subIndex(D,dir,orient)
	
  override def defineVelocity(u:Array[Double]) = {uBC = u}

  def rho( f: Array[Double]) : Double = {
		// rhoOnWall is the sum of f_i s that are perperdicular to the wall's normal
		val rhoOnWall = Indexes.extractSubArray(f,onWallIndices).reduceLeft(_+_)
		// rhoOnWall is the sum of f_i s that are parallel to the wall's normal
		val rhoNormal = Indexes.extractSubArray(f,normalIndices).reduceLeft(_+_)
    (2.0*rhoNormal+rhoOnWall) / (1.0+orient * uBC(dir))
  }
  
  def u( f: Array[Double], rho: Double ) = uBC
  
  def deviatoricStress(f:Array[Double], density:Double, vel:Array[Double]) : Array[Double] = {
    val uSqr = Arrays.normSqr(uBC)
    
    val fNeq = new Array[Double](D.q)
    for (iPop <- onWallIndices) fNeq(iPop) = f(iPop) - equilibrium(iPop, density, uBC, uSqr)
    
    for (iPop <- normalIndices) {
      if (iPop == 0) fNeq(iPop) = 0
      else fNeq(iPop) = f(iPop) - equilibrium(iPop, density, uBC, uSqr)
    }
    var iPi = 0
    val piNeq = new Array[Double](D.n)
    var iA = 0
    while( iA < D.d ) {
      var iB = iA
      while( iB < D.d ) {
        for (iPop <- onWallIndices) piNeq(iPi) += D.c(iPop)(iA)*D.c(iPop)(iB)*fNeq(iPop)
          for (iPop <- normalIndices) piNeq(iPi) += 2.0*D.c(iPop)(iA)*D.c(iPop)(iB)*fNeq(iPop)
            iPi += 1
        iB += 1
      }
      iA += 1
    }
    piNeq
  }
}

class RegularizedVelocityBoundaryCondition (D:Descriptor, 
					    dir:Int, orient:Int) extends DirichletVelocityDynamics(D,dir,orient) {
           
  def copy() = {
    val tmpDyn = new RegularizedVelocityBoundaryCondition(D, dir, orient)
    tmpDyn.baseDyn = baseDyn
    tmpDyn.uBC = uBC
    tmpDyn
  }
	
	def completePopulations(f:Array[Double]) {
		val density = rho(f)
		val vel = u(f,density)
		val piNeq = deviatoricStress(f,density,vel)
    for (iPop <- D.popIndices) f(iPop) = regularize(iPop, density, vel, piNeq)
	}
  
  override lazy val toString = "RegularizedVelocityBC("+D+", baseDyn="+baseDyn.toString+")"

}
