package lb.multiUtil

import lb.select._
import lb.util._

import Math._

object MultiUtils {

	def getNextFlatDomain(nextLattice:Lattice2D,dir:Int,orient:Int) : Rectangle = {
		val vicinity = nextLattice.D.vicinity
		val nx = nextLattice.nX-1
		val ny = nextLattice.nY-1
		val nextDomain = {
			if (dir == 0) {
				if (orient == 1) new Rectangle(0,vicinity-1,vicinity,ny-vicinity)
				else new Rectangle(nx-(vicinity-1),nx,vicinity,ny-vicinity)
			}
			else {
				if (orient == 1) new Rectangle(vicinity,nx-vicinity,0,vicinity-1)
				else new Rectangle(vicinity,nx-vicinity,ny-(vicinity-1),ny)
			}
		}
		
		nextDomain
	}

	def getNextCornerDomain(nextLattice:Lattice2D,xNormal:Int,yNormal:Int) : Rectangle = {
		val vicinity = nextLattice.D.vicinity
		val nx = nextLattice.nX-1
		val ny = nextLattice.nY-1
		val nextDomain = {
			if (xNormal == 1) {
				if (yNormal == 1) new Rectangle(0,vicinity-1,0,vicinity-1)
				else new Rectangle(0,vicinity-1,ny-(vicinity-1),ny)
			}
			else {
				if (yNormal == 1) new Rectangle(nx-(vicinity-1),nx,0,vicinity-1)
				else new Rectangle(nx-(vicinity-1),nx,ny-(vicinity-1),ny)
			}
		}

		nextDomain
	}

	def copyCellsDomain(lattice:Lattice2D,domain:Rectangle,cellsDomain:Array[Array[Cell]]) : Unit = {
		for (iX <- domain.fromX to domain.toX; iY <- domain.fromY to domain.toY) {
			val relX = iX - domain.fromX
			val relY = iY - domain.fromY
			lattice(iX,iY).dyn = cellsDomain(relX)(relY).dyn.copy
			for (iPop <- 0 until lattice.D.q) lattice(iX,iY)(iPop) = cellsDomain(relX)(relY)(iPop)
		}
	}
    
    
}