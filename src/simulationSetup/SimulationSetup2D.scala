package lb.simSetup

import lb.util._

object SimSetup {
  private def equilibriumInitializion[T <: Descriptor](D:T,fRho : (Int,Int) => Double, fU : (Int, Int) => Array[Double])
  (iX:Int, iY:Int, cell:Cell[T]) = {
    val density = fRho(iX,iY)
    val velocity = fU(iX,iY)
    val velSqr = Arrays.normSqr(velocity)
    
    for (iPop <- 0 until D.q) {
      cell(iPop) = cell.dyn.equilibrium(iPop,density,velocity,velSqr)
    }
  }

  def iniAtEquilibrium[T <: Descriptor](D:T,
					fRho : (Int,Int) => Double, 
					fU : (Int, Int) => Array[Double]
				      ): (Int,Int,Cell[T]) => Unit = {
    equilibriumInitializion(D,fRho,fU)_ 
  }
  
  def velocityDefinition[T <: Descriptor](D:T,f: (Int,Int) => Array[Double])(iX:Int,iY:Int,cell:Cell[T]) {
    cell.defineVelocity(f(iX,iY))
  }
  
  def defineVelocity[T <: Descriptor](D:T,fU : (Int, Int) => Array[Double]): (Int,Int,Cell[T]) => Unit = {
    velocityDefinition(D,fU)_ 
  }
}


