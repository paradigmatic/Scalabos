package lb.dyn

import lb.util._

object lbHelpers {
    def computeU(D:Descriptor,f:Array[Double],rho:Double) = {
        if (D == D2Q9) uD2Q9(f,rho)
        else uGeneric(D,f,rho)
    }
    
    private def uD2Q9(f:Array[Double],rho:Double) : Array[Double]= {
        val u = new Array[Double](2)
        val invRho = 1.0 / rho
        u(0) = (-f(1)-f(2)-f(3)+f(5)+f(6)+f(7)) * invRho
        u(1) = ( f(1)-f(3)-f(4)-f(5)+f(7)+f(8)) * invRho
        u
    }
    
    private def uGeneric(D:Descriptor,f:Array[Double],rho:Double) : Array[Double] = {
        val vel = new Array[Double](D.d)
        for( iPop <- D.popIndices; iD <- D.dimIndices) vel(iD) += D.c(iPop)(iD)*f(iPop)
        vel.map(_ / rho)
    }
    
    def bgkCollision(D:Descriptor,f:Array[Double],rho:Double,u:Array[Double],omega:Double) = {
        if (D == D2Q9) bgkCollisionD2Q9(D,f,rho,u,omega)
        else bgkCollisionGeneric(D,f,rho,u,omega)
    }
    
    private def bgkCollisionD2Q9(D:Descriptor,f:Array[Double],rho:Double,u:Array[Double],omega:Double) = {
        val omRho = omega * rho
        
        val omRho_t0 = omRho * D.t(0)
        val omRho_t1 = omRho * D.t(1)
        val omRho_t2 = omRho * D.t(2)
        
        val oneMinusOmega = 1.0 - omega
        
        val uSqr = u(0)*u(0)+u(1)*u(1)
        val oneMinusUsqr = 1.0-1.5*uSqr
        f(0) *= oneMinusOmega; f(0) += omRho_t0 * oneMinusUsqr
        
        var c_u = 3.0*(-u(0)+u(1))
        var half_c_u_sqr = 0.5*c_u*c_u
        f(1) *= oneMinusOmega; f(1) += omRho_t1 * (oneMinusUsqr + c_u +half_c_u_sqr)
        f(5) *= oneMinusOmega; f(5) += omRho_t1 * (oneMinusUsqr - c_u +half_c_u_sqr)
        
        c_u = 3.0*(-u(0))
        half_c_u_sqr = 0.5*c_u*c_u
        f(2) *= oneMinusOmega; f(2) += omRho_t2 * (oneMinusUsqr + c_u +half_c_u_sqr)
        f(6) *= oneMinusOmega; f(6) += omRho_t2 * (oneMinusUsqr - c_u +half_c_u_sqr)
        
        c_u = 3.0*(-u(0)-u(1))
        half_c_u_sqr = 0.5*c_u*c_u
        f(3) *= oneMinusOmega; f(3) += omRho_t1 * (oneMinusUsqr + c_u +half_c_u_sqr)
        f(7) *= oneMinusOmega; f(7) += omRho_t1 * (oneMinusUsqr - c_u +half_c_u_sqr)
        
        c_u = 3.0*(-u(1))
        half_c_u_sqr = 0.5*c_u*c_u
        f(4) *= oneMinusOmega; f(4) += omRho_t2 * (oneMinusUsqr + c_u +half_c_u_sqr)
        f(8) *= oneMinusOmega; f(8) += omRho_t2 * (oneMinusUsqr - c_u +half_c_u_sqr)
    }
    
    private def bgkCollisionGeneric(D:Descriptor,f:Array[Double],rho:Double,u:Array[Double],omega:Double) = {
        val velSqr = Arrays.normSqr(u)
        
        def equilibrium(iPop:Int, rho:Double, u:Array[Double], uSqr:Double) : Double = {
            var c_u = Arrays.dot(D.c(iPop),u)
            c_u *= D.invCs2
            
            D.t(iPop)*rho*(1.0 + c_u + 0.5 * (c_u*c_u - D.invCs2*uSqr ))
        }

        for (iPop <- D.popIndices) { 
            f(iPop) *= (1.0 - omega)
            f(iPop) += omega*equilibrium(iPop,rho,u,velSqr)
        }
    }
    
}