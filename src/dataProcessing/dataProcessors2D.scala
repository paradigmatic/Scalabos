package lb.dataProcessors2D

import lb.select._
import lb.util._

object Averages {
  
  def density[T <: Descriptor](lattice:Lattice2D[T], domain:Region) : Double  = {
    //     var totEnergy = 0.5 * lattice.map( C => Arrays.normSqr(C.u)).flatMap( x => x ).reduceLeft(_+_)
    val latticeDomain = lattice.select(domain)
    val totCells = latticeDomain.indices.size
    
    var totEnergy = latticeDomain.map( C => C.rho).reduceLeft(_+_)
    totEnergy /= totCells
    totEnergy
  }
  
  def energy[T <: Descriptor](lattice:Lattice2D[T], domain:Region) : Double  = {
    val latticeDomain = lattice.select(domain)
    val totCells = latticeDomain.indices.size

    var totEnergy = 0.5 * latticeDomain.map( C => Arrays.normSqr(C.u)).reduceLeft(_+_)
    totEnergy /= totCells
    totEnergy
  }
  
}

