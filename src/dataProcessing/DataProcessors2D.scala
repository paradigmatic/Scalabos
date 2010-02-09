package lb.dataProcessors2D

import lb.select._
import lb.util._

abstract class DataProcessor2D(val lattice:Lattice2D, val domain:Region) {
  def apply() = { lattice.select(domain).foreach( (iX,iY,C) => process(iX,iY,C) ) }
  def process(iX:Int,iY:Int,cell:Cell) : Unit
}

object dataProcessorsInterfaces {
  
  def addDataProcessor(lattice:Lattice2D,dataProc:DataProcessor2D) {
    lattice.dataProcessors = lattice.dataProcessors ::: List(dataProc)
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

