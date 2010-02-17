package lb.dyn

import lb.select._
import lb.util._
import lb.dataProcessors2D._

object DynInterfaces {
  
	// Lattice 2D
  def defineDynamics(lattice:Lattice2D, domain:Rectangle, dynamics:Dynamics) : Unit = {
    lattice.select(domain).foreach(_.defineDynamics(dynamics))
  }
  
  // MultiLattice 2D
  def defineDynamics(lattice:MultiLattice2D, domain:Rectangle, dynamics:Dynamics) : Unit = {
		for (iNx <- 0 until lattice.nBlockX ; iNy <- 0 until lattice.nBlockY) {
			var overlap = Domains2D.getDomainIntersection(domain, lattice.blockBoundingBox(iNx)(iNy))
			if (overlap != None) {
				//print "Before : "
				//print overlap.fromX, overlap.toX, overlap.fromY, overlap.toY
				overlap = overlap.shift(new Dot2D(-lattice.getBlock(iNx,iNy).offset.x,-lattice.getBlock(iNx,iNy).offset.y))
				//print "After : "
				//print overlap.fromX, overlap.toX, overlap.fromY, overlap.toY
				defineDynamics(lattice.getBlock(iNx,iNy),overlap,dynamics)
			}
		}
  }
  
  // Lattice 2D
  
  def addVelocityBoundaryConditionOnBoundingBox(lattice:Lattice2D) {
    val ww = Rectangle( 0,            0,            1,            lattice.nY-2 )
    val ew = Rectangle( lattice.nX-1, lattice.nX-1, 1,            lattice.nY-2 )
    val sw = Rectangle( 1,            lattice.nX-2, 0,            0 )
    val nw = Rectangle( 1,            lattice.nX-2, lattice.nY-1, lattice.nY-1 )
    
    addVelocityBoundary(lattice,ww,new RegularizedVelocityBoundaryCondition(D2Q9, 0, -1))
    addVelocityBoundary(lattice,ew,new RegularizedVelocityBoundaryCondition(D2Q9, 0, +1))
    addVelocityBoundary(lattice,sw,new RegularizedVelocityBoundaryCondition(D2Q9, 1, -1))
    addVelocityBoundary(lattice,nw,new RegularizedVelocityBoundaryCondition(D2Q9, 1, +1))
    
    val nwc = Rectangle( 0,            0,            lattice.nY-1, lattice.nY-1 )
    val swc = Rectangle( 0,            0,            0,            0 )
    val nec = Rectangle( lattice.nX-1, lattice.nX-1, lattice.nY-1, lattice.nY-1 )
    val sec = Rectangle( lattice.nX-1, lattice.nX-1, 0,            0 )
    
    addExternalVelocityCornerBoundary(lattice,nwc,-1,+1)
    addExternalVelocityCornerBoundary(lattice,swc,-1,-1)
    addExternalVelocityCornerBoundary(lattice,nec,+1,+1)
    addExternalVelocityCornerBoundary(lattice,sec,+1,-1)
  }
  
  // Multi Lattice 2D
  
  def addVelocityBoundaryConditionOnBoundingBox(lattice:MultiLattice2D) {
    val ww = Rectangle( 0,            0,            1,            lattice.nY-2 )
    val ew = Rectangle( lattice.nX-1, lattice.nX-1, 1,            lattice.nY-2 )
    val sw = Rectangle( 1,            lattice.nX-2, 0,            0 )
    val nw = Rectangle( 1,            lattice.nX-2, lattice.nY-1, lattice.nY-1 )
    
    addVelocityBoundary(lattice,ww,new RegularizedVelocityBoundaryCondition(D2Q9, 0, -1))
    addVelocityBoundary(lattice,ew,new RegularizedVelocityBoundaryCondition(D2Q9, 0, +1))
    addVelocityBoundary(lattice,sw,new RegularizedVelocityBoundaryCondition(D2Q9, 1, -1))
    addVelocityBoundary(lattice,nw,new RegularizedVelocityBoundaryCondition(D2Q9, 1, +1))
    
    val nwc = Rectangle( 0,            0,            lattice.nY-1, lattice.nY-1 )
    val swc = Rectangle( 0,            0,            0,            0 )
    val nec = Rectangle( lattice.nX-1, lattice.nX-1, lattice.nY-1, lattice.nY-1 )
    val sec = Rectangle( lattice.nX-1, lattice.nX-1, 0,            0 )
    
    addExternalVelocityCornerBoundary(lattice,nwc,-1,+1)
    addExternalVelocityCornerBoundary(lattice,swc,-1,-1)
    addExternalVelocityCornerBoundary(lattice,nec,+1,+1)
    addExternalVelocityCornerBoundary(lattice,sec,+1,-1)
  }
  
  // Lattice 2D case 
  
  def addVelocityBoundary(lattice:Lattice2D,domain:Rectangle,bcDyn:DirichletVelocityDynamics) {
    lattice.select(domain).foreach( C => {
      val baseDyn = C.dyn
      bcDyn.defineBaseDynamics(baseDyn)
      C.defineDynamics(bcDyn.copy)} )
  }
  
  // MultiLattice 2D case 
  
  def addVelocityBoundary(lattice:MultiLattice2D,domain:Rectangle,bcDyn:DirichletVelocityDynamics) {
		for (iNx <- 0 until lattice.nBlockX ; iNy <- 0 until lattice.nBlockY) {
			var overlap = Domains2D.getDomainIntersection(domain, lattice.blockBoundingBox(iNx)(iNy))
			if (overlap != None) {
				//print "Before : "
				//print overlap.fromX, overlap.toX, overlap.fromY, overlap.toY
				overlap = overlap.shift(new Dot2D(-lattice.getBlock(iNx,iNy).offset.x,-lattice.getBlock(iNx,iNy).offset.y))
				addVelocityBoundary(lattice.getBlock(iNx,iNy),overlap,bcDyn)
			}
		}
  }
  
  
  // Lattice 2D case 
  
  def addExternalVelocityCornerBoundary(lattice:Lattice2D,domain:Rectangle,xNormal:Int,yNormal:Int) {
    defineDynamics(lattice,domain,new ImposedDensityAndVelocityDynamics(lattice.D))
    val dataProc = new CornerBoundaryConditionProcessor2D(lattice,domain,xNormal,yNormal)
    DataProcessorsInterfaces.addDataProcessor(lattice,dataProc)
  }
  
  // MultiLattice 2D case 
  
  def addExternalVelocityCornerBoundary(lattice:MultiLattice2D,domain:Rectangle,xNormal:Int,yNormal:Int) {
    defineDynamics(lattice,domain,new ImposedDensityAndVelocityDynamics(lattice.D))
    
    for (iNx <- 0 until lattice.nBlockX ; iNy <- 0 until lattice.nBlockY) {
			var overlap = Domains2D.getDomainIntersection(domain, lattice.blockBoundingBox(iNx)(iNy))
			if (overlap != None) {
				//print "Before : "
				//print overlap.fromX, overlap.toX, overlap.fromY, overlap.toY
				overlap = overlap.shift(new Dot2D(-lattice.getBlock(iNx,iNy).offset.x,-lattice.getBlock(iNx,iNy).offset.y))
				val dataProc = new CornerBoundaryConditionProcessor2D(lattice.getBlock(iNx,iNy),overlap,xNormal,yNormal)
				DataProcessorsInterfaces.addDataProcessor(lattice.getBlock(iNx,iNy),dataProc)
			}
		}
  }
  
}
