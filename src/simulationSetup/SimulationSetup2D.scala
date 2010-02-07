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
  
  private def firstOrderInitializion(D: Descriptor,fRho : (Int,Int) => Double, 
                                     fU : (Int, Int) => Array[Double], 
                                     fPiNeq : (Int, Int) => Array[Double])(iX:Int, iY:Int, cell:Cell) = {
    val density = fRho(iX,iY)
    val velocity = fU(iX,iY)
    val piNeq = fPiNeq(iX,iY)
      
    for (iPop <- 0 until D.q) {
      cell(iPop) = cell.dyn.regularize(iPop,density,velocity,piNeq)
    }
  }
  
  def iniAtFirstOrder(D: Descriptor,
                      fRho : (Int,Int) => Double, 
                      fU : (Int, Int) => Array[Double],
                      fPiNeq : (Int, Int) => Array[Double]) : (Int,Int,Cell) => Unit = {
                         firstOrderInitializion(D,fRho,fU,fPiNeq)_ 
                       }
  
  def velocityDefinition(D:Descriptor,f: (Int,Int) => Array[Double])(iX:Int,iY:Int,cell:Cell) {
    cell.defineVelocity(f(iX,iY))
  }
  
  def defineVelocity(D:Descriptor,fU : (Int, Int) => Array[Double]): (Int,Int,Cell) => Unit = {
    velocityDefinition(D,fU)_ 
  }
}


