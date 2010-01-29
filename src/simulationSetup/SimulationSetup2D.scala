package lb.simSetup

import lb.util._

object SimSetup {
  private def equilibriumInitializion(D: Descriptor,fRho : (Int,Int) => Double, fU : (Int, Int) => Array[Double])
  (iX:Int, iY:Int, cell:Cell) = {
    val density = fRho(iX,iY)
    val velocity = fU(iX,iY)
    val velSqr = Arrays.normSqr(velocity)
    
    for (iPop <- 0 until D.q) {
      cell(iPop) = cell.dyn.equilibrium(iPop,density,velocity,velSqr)
    }
  }

  def iniAtEquilibrium(D: Descriptor,
					fRho : (Int,Int) => Double, 
					fU : (Int, Int) => Array[Double]
				      ): (Int,Int,Cell) => Unit = {
    equilibriumInitializion(D,fRho,fU)_ 
  }
  
  def velocityDefinition(D:Descriptor,f: (Int,Int) => Array[Double])(iX:Int,iY:Int,cell:Cell) {
    cell.defineVelocity(f(iX,iY))
  }
  
  def defineVelocity(D:Descriptor,fU : (Int, Int) => Array[Double]): (Int,Int,Cell) => Unit = {
    velocityDefinition(D,fU)_ 
  }
}


