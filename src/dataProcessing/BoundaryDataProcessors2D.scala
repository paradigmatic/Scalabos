package lb.dataProcessors2D

import lb.select._
import lb.util._

class CornerBoundaryConditionProcessor2D(lattice:Lattice2D,domain:Region,val xNormal:Int, val yNormal:Int) 
    extends DataProcessor2D(lattice.D,lattice,domain) {
  def process(iX:Int, iY:Int, cell:Cell) : Unit = {
    
    val rho10 = lattice(iX-1*xNormal, iY-0*yNormal).rho
    val rho01 = lattice(iX-0*xNormal, iY-1*yNormal).rho
    
    val rho = 0.5*(rho01+rho10);
    val u = cell.u
    val cell_x1 = lattice(iX-xNormal,iY)
    val cell_y1 = lattice(iX,iY-yNormal)
    val u_x1 = cell_x1.u
    val u_y1 = cell_y1.u
    
    val dx_u = new Array[Double](D.d)
    for (iD <- D.dimIndices) dx_u(iD) = Fd.fwdDiff(u_x1(iD),u(iD),-xNormal)
    val dy_u = new Array[Double](D.d)
    for (iD <- D.dimIndices) dy_u(iD) = Fd.fwdDiff(u_y1(iD),u(iD),-yNormal)
    
    val dx_ux = dx_u(0)
    val dy_ux = dy_u(0)
    val dx_uy = dx_u(1)
    val dy_uy = dy_u(1)
    
    val omega = cell.dyn.omega
    
    val sToPi = - rho * D.cs2 / omega
    
    val piNeq = new Array[Double](D.n)
    piNeq(Indexes.dim2.xx) = (2 * dx_ux * sToPi)       
    piNeq(Indexes.dim2.xy) = ((dx_uy + dy_ux) * sToPi) 
    piNeq(Indexes.dim2.yy) = (2 * dy_uy * sToPi)       
    
    for (iPop <- D.popIndices) cell(iPop) = cell.dyn.regularize(iPop,rho,u,piNeq)
  }
}
