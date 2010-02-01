package lb.dataProcessors2D

import lb.select._
import lb.util._
        

// class CornerBoundaryConditionProcessor2D(override val lattice:Lattice2D,val xNormal:Int, val yNormal:Int) 
//     extends DataProcessor2D(lattice) {
//   def apply() = {
//     
//     for iX in range(domain.x0,domain.x1+1) {
//       for iY in range(domain.y0,domain.y1+1) {
//         val rho10 = lattice(iX-1*xNormal, iY-0*yNormal).rho
//         val rho01 = lattice(iX-0*xNormal, iY-1*yNormal).rho
//         
//         val rho = 0.5*(rho01+rho10);
//         val u = lattice(iX,iY).u(rho)
//         val cell_x1 = lattice(iX-xNormal,iY)
//         val cell_y1 = lattice(iX,iY-yNormal)
//         val u_x1 = cell_x1.compute_u(cell_x1.compute_rho())
//         val u_y1 = cell_y1.compute_u(cell_y1.compute_rho())
//         
//         val dx_u = new Array[Double](D.d)
//         for (iD <- 0 until D.d) dx_u(iD) = -xNormal*(u_x1(iD)-u(iD))
//         val dy_u = new Array[Double](D.d)
//         for (iD <- 0 until D.d) dy_u(iD) = -yNormal*(u_y1(iD)-u(iD))
//         
//         val dx_ux = dx_u(0)
//         val dy_ux = dy_u(0)
//         val dx_uy = dx_u(1)
//         val dy_uy = dy_u(1)
//         
//         val cell = lattice(iX,iY)
//         val cell.dynamics.define_rho(rho)
//         val omega = cell.dyn.omega
//         
//         val sToPi = - rho * lattice.D.cs2 / omega
//         
//         val piNeq = new Array[Double](D.n)
//         val piNeq.append(2 * dx_ux * sToPi)       # index = 1 -> xx
//         val piNeq.append((dx_uy + dy_ux) * sToPi) # index = 2 -> xy
//         val piNeq.append(2 * dy_uy * sToPi)       # index = 3 -> yy
//         
//         for ipop in range(lattice.descriptor.q):
//           cell[ipop] = cell.dynamics.regularize(ipop,rho,u,piNeq)
//           return
//       }
//     }
//     
//   }
// }
