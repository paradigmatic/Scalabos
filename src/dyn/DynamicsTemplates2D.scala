package lb.dyn.helpers2d

import lb.util._

object lbHelpers2D {
  
  // methods for the computation of the density, velocity and bgk collision
  // in the D2Q9 special case.
  
  def rhoD2Q9(f:Array[Double]) = {
    f(0)+f(1)+f(2)+f(3)+f(4)+f(5)+f(6)+f(7)+f(8)
  }
  
  def uD2Q9(f:Array[Double],rho:Double) : Array[Double]= {
    val u = new Array[Double](2)
    val invRho = 1.0 / rho
    u(0) = (-f(1)-f(2)-f(3)+f(5)+f(6)+f(7)) * invRho
    u(1) = ( f(1)-f(3)-f(4)-f(5)+f(7)+f(8)) * invRho
    u
  }
  
  def equilibriumD2Q9(iPop:Int, rho:Double, u:Array[Double], uSqr:Double) = {
    val c_u = 3.0 * (D2Q9.c(iPop)(0)*u(0) + D2Q9.c(iPop)(1)*u(1))
    
    D2Q9.t(iPop)*rho*(1.0 + c_u + 0.5 * (c_u*c_u - 3.0*uSqr ))
  }
  
  def bgkCollisionD2Q9(f:Array[Double],rho:Double,u:Array[Double],omega:Double) = {
    val omRho = omega * rho
    
    val omRho_t0 = omRho * D2Q9.t(0)
    val omRho_t1 = omRho * D2Q9.t(1)
    val omRho_t2 = omRho * D2Q9.t(2)
    
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
    
}