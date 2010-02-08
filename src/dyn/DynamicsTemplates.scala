package lb.dyn.helpers

import lb.dyn.helpers2d._
import lb.util._

object lbHelpers {
		def computeRho(D:Descriptor,f:Array[Double]) = {
      if (D == D2Q9) lbHelpers2D.rhoD2Q9(f)
      else lbHelpersGenerics.rho(f)
    }
	
    def computeU(D:Descriptor,f:Array[Double],rho:Double) = {
      if (D == D2Q9) lbHelpers2D.uD2Q9(f,rho)
      else lbHelpersGenerics.u(D,f,rho)
    }
    
    def bgkCollision(D:Descriptor,f:Array[Double],rho:Double,u:Array[Double],omega:Double) = {
      if (D == D2Q9) lbHelpers2D.bgkCollisionD2Q9(f,rho,u,omega)
      else lbHelpersGenerics.bgkCollision(D,f,rho,u,omega)
    }

  def equilibrium(D:Descriptor,iPop:Int, rho:Double, u:Array[Double], uSqr:Double) = {
      if (D == D2Q9) lbHelpers2D.equilibriumD2Q9(iPop,rho,u,uSqr)
      else lbHelpersGenerics.equilibrium(D,iPop,rho,u,uSqr)
    }
}

object lbHelpersGenerics {
  def rho(f:Array[Double]) = {
    f.reduceLeft(_+_)
  }
  
  def u(D:Descriptor,f:Array[Double],rho:Double) : Array[Double] = {
    val vel = new Array[Double](D.d)
    for( iPop <- D.popIndices; iD <- D.dimIndices) vel(iD) += D.c(iPop)(iD)*f(iPop)
      vel.map(_ / rho)
  }
  
  def equilibrium(D:Descriptor,iPop:Int, rho:Double, u:Array[Double], uSqr:Double) : Double = {
    var c_u = Arrays.dot(D.c(iPop),u)
    c_u *= D.invCs2
    
    D.t(iPop)*rho*(1.0 + c_u + 0.5 * (c_u*c_u - D.invCs2*uSqr ))
  }
  
  def bgkCollision(D:Descriptor,f:Array[Double],rho:Double,u:Array[Double],omega:Double) = {
    val velSqr = Arrays.normSqr(u)
    
    for (iPop <- D.popIndices) { 
      f(iPop) *= (1.0 - omega)
      f(iPop) += omega*equilibrium(D,iPop,rho,u,velSqr)
    }
  }
  
  
}