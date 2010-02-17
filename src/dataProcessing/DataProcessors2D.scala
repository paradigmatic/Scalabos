package lb.dataProcessors2D

import lb.select._
import lb.util._

abstract class DataProcessor2D(var lattice:Lattice2D, var domain:Rectangle) {
  def apply() = { lattice.select(domain).foreach( (iX,iY,C) => process(iX,iY,C) ) }
  def process(iX:Int,iY:Int,cell:Cell) : Unit
  
  def defineDomain(dom:Rectangle) = { domain = dom }
  def defineLattice(latt:Lattice2D) = { lattice = latt }
}

object DataProcessorsInterfaces {
  
  def addDataProcessor(lattice:Lattice2D,dataProc:DataProcessor2D) : Unit = {
    lattice.dataProcessors = lattice.dataProcessors ::: List(dataProc)
  }
  
  def addDataProcessor(lattice:MultiLattice2D,domain:Rectangle,dataProc:DataProcessor2D) : Unit = {
		for (iNx <- 0 until lattice.nBlockX ; iNy <- 0 until lattice.nBlockY) {
			var overlap = Domains2D.getDomainIntersection(domain, lattice.blockBoundingBox(iNx)(iNy))
			if (overlap != None) {
				//print "Before : "
				//print overlap.fromX, overlap.toX, overlap.fromY, overlap.toY
				overlap.shift(new Dot2D(-lattice.getBlock(iNx,iNy).offset.x,-lattice.getBlock(iNx,iNy).offset.y))
				//print "After : "
				//print overlap.fromX, overlap.toX, overlap.fromY, overlap.toY
				dataProc.defineDomain(new Rectangle(overlap.fromX,overlap.toX,overlap.fromY,overlap.toY))
				dataProc.defineLattice(lattice.getBlock(iNx,iNy))
				addDataProcessor(lattice.getBlock(iNx,iNy),dataProc)
			}
		}
	}
  
}

object Averages {
  
  def density(lattice:Lattice2D, domain:Region) : Double  = {
    val latticeDomain = lattice.select(domain)
    val totCells = latticeDomain.indices.size
    
    var totEnergy = latticeDomain.map( C => C.rho).reduceLeft(_+_)
    totEnergy /= totCells
    totEnergy
  }
  
  def energy(lattice:Lattice2D, domain:Region) : Double  = {
    val latticeDomain = lattice.select(domain)
    val totCells = latticeDomain.indices.size

    var totEnergy = 0.5 * latticeDomain.map( C => Arrays.normSqr(C.u)).reduceLeft(_+_)
    totEnergy /= totCells
    totEnergy
  }
  
  def velocityL2Error(lattice:Lattice2D, fU : (Int,Int) => Array[Double]) : Double = {
    var l2error = lattice.map( (iX,iY,C) => Arrays.normSqr(Arrays.diff(C.u,fU(iX,iY))) ).flatMap(x=>x).reduceLeft(_+_)
    l2error /= (lattice.nX * lattice.nY)
    l2error = Math.sqrt(l2error)
    l2error
  }
}

object MultiAverages {
  def velocityL2Error(lattice:MultiLattice2D, fU : (Int,Int) => Array[Double]) : Double = {
    var l2error = lattice.map( (iX,iY,C) => Arrays.normSqr(Arrays.diff(C.u,fU(iX,iY))) ).flatMap(x=>x).reduceLeft(_+_)
    l2error /= (lattice.nX * lattice.nY)
    l2error = Math.sqrt(l2error)
    l2error
  }
  
}

