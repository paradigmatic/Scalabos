package lb.dyn

import lb.util._

abstract class compositeDynamics[T <: Descriptor](override val D:T) extends Dynamics(D) {
	var baseDyn:Dynamics[T] = new NoDynamics(D)
	
	def defineBaseDyn(dyn:Dynamics[T]) = { baseDyn = dyn }
	
	def completePopulations(f:Array[Double]) : Unit
	
	override def apply(f:Array[Double]) = {
		completePopulations(f)
		baseDyn(f)
	}
}



abstract class DirichletVelocityDynamics[T <: Descriptor](override val D:T, vel:Array[Double], val dir:Int, val orient:Int) extends compositeDynamics(D) {

	var uBC = vel
	
	lazy val onWallIndices = Indexes.subIndex(D,dir,0)
	lazy val normalIndices = Indexes.subIndex(D,dir,orient)
	
	def defineU(vel:Array[Double]) = {uBC = vel}

  def rho( f: Array[Double]) : Double = {
		// rhoOnWall is the sum of f_i s that are perperdicular to the wall's normal
		val rhoOnWall = Indexes.extractSubArray(f,onWallIndices).reduceLeft(_+_)
		// rhoOnWall is the sum of f_i s that are parallel to the wall's normal
		val rhoNormal = Indexes.extractSubArray(f,normalIndices).reduceLeft(_+_)
    (2*rhoNormal+rhoOnWall) / (1.0+orient * uBC(dir))
  }
  
  def u( f: Array[Double], rho: Double ) = uBC
}

abstract class RegularizedVelocityBoundaryCondition[T <: Descriptor]
							 (override val D:T, vel:Array[Double], 
								override val dir:Int, override val orient:Int) extends DirichletVelocityDynamics(D,vel,dir,orient) {
									
	def deviatoricStress(f:Array[Double], density:Double, vel:Array[Double]) : Array[Double] = {
		val uSqr = Arrays.normSqr(uBC)

		val fNeq = new Array[Double](D.q)
		for (iPop <- onWallIndices) fNeq(iPop) = f(iPop) - baseDyn.equilibrium(iPop, density, uBC, uSqr)
				
		for (iPop <- normalIndices) {
				if (iPop == 0) fNeq(iPop) = 0
				else fNeq(iPop) = f(iPop) - baseDyn.equilibrium(iPop, density, uBC, uSqr)
		}
		var iPi = 0
		val piNeq = new Array[Double](D.n)
		for (iA <- 0 until D.d; iB <- iA until D.d ) {
			for (iPop <- onWallIndices) piNeq(iPi) += D.c(iPop)(iA)*D.c(iPop)(iB)*fNeq(iPop)
			for (iPop <- normalIndices) piNeq(iPi) += 2*D.c(iPop)(iA)*D.c(iPop)(iB)*fNeq(iPop)
			iPi += 1
		}
		piNeq
	}
	
	def completePopulations(f:Array[Double]) {
		val density = rho(f)
		val vel = u(f,density)
		val piNeq = deviatoricStress(f,density,vel)
		piNeq
// 		f = baseDyn.regularize(density,vel,piNeq)
	}

}