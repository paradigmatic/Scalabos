package lb

import lb.dataProcessors2D._
import lb.dyn._
import lb.util._
import lb.multiUtil._

class MultiLattice2D(val nX:Int,val nY:Int,val defaultDyn:Dynamics,val nBlockX:Int,val nBlockY:Int) {
	lazy val D = defaultDyn.D
	private var dataProcessors = List[DataProcessor2D]()
	val boundingBox = new Box2D(0,nX-1,0,nY-1)
  
  private val nxSizes = determineSizes(nX,nBlockX)
  private val nySizes = determineSizes(nY,nBlockY)
  
	def determineSizes(n:Int,nBlock:Int) = {
		val meanN = n / nBlock
		val rest  = n % nBlock

		val nList = {
      val list = new Array[Int](nBlock)
      for (iN <- 0 until nBlock) list(iN) = meanN
        list
    }
		for (i <- 0 until rest) nList(i) += 1
			
		nList
	}
	
	private val blockGrid = {
		val g = new Array[Array[Lattice2D]](nBlockX,nBlockY)
		for (iX <- 0 until nBlockX; iY <- 0 until nBlockY) {
			g(iX)(iY) = new Lattice2D(D,nxSizes(iX)+2*D.vicinity,nySizes(iY)+2*D.vicinity,defaultDyn)
		}
		g
	}
	
	private val blockBoundingBox = {
		var x0 = 0; var x1 = 0
		val bbox = new Array[Array[Box2D]](nBlockX,nBlockY)
		for (iX <- 0 until nBlockX) {
			val x1 = x0 + nxSizes(iX) - 1
			var y0 = 0; var y1 = 0
			for (iY <- 0 until nBlockY) {
				val y1 = y0 + nySizes(iY) - 1
				bbox(iX)(iY) = new Box2D(x0,x1,y0,y1)
				blockGrid(iX)(iY).offset = new Dot2D(x0-D.vicinity,y0-D.vicinity)
				y0 = y1 + 1
			}
			x0 = x1 + 1
		}
		bbox
	}

	def getBlock(inX:Int,inY:Int) : Lattice2D = blockGrid(inX)(inY)

	def defineDynamics(dynamics:Dynamics,iX:Int,iY:Int) : Unit = {
		val block = determineBlock(iX,iY)
		val offset = determineOffset(block,iX,iY)
		blockGrid(block.x)(block.y).defineDynamics(offset.x,offset.y,dynamics)
	}

	def defineDynamics(dynamics:Dynamics,domain:Box2D) : Unit = {
		for (iX <- domain.x0 to domain.x1; iY <- domain.y0 to domain.y1) {
			defineDynamics(dynamics,iX,iY)
		}
	}

	def determineBlock(iX:Int,iY:Int) = {
			var totX = 0; var totY = 0
			val block = { 
				var x = 0; var y = 0
				for (iNx <- 0 until nBlockX) {
// 					print iX, nxSizes[iNx]-1
					if ((iX >= totX) && (iX <= totX + (nxSizes(iNx)-1))) x = iNx
					else totX += nxSizes(iNx)
			}
			for (iNy <- 0 until nBlockY) {
					//print iY, totY, iNy*(nySizes[iNy]-1)
					if ((iY >= totY) && (iY <= totY + (nySizes(iNy)-1))) y = iNy
					else totY += nySizes(iNy)
			}
			new Dot2D(x,y)
		}
		block
	}

	def determineOffset(block:Dot2D,iX:Int,iY:Int) = {
		//print iX, iY
		var offsetX = D.vicinity
		var offsetY = D.vicinity
		for (inX <- 0 until block.x) offsetX -= nxSizes(inX)
		for (inY <- 0 until block.y) offsetY -= nySizes(inY)

		new Dot2D(iX,iY).shift(new Dot2D(offsetX,offsetY))
	}


	def apply(iX:Int,iY:Int) : Cell = {
			val block = determineBlock(iX,iY)
			val offset = determineOffset(block,iX,iY)
			//print nxSizes, nySizes
			//print "multi get, points = ", iX,iY, "block = ", block.x, block.y, "offset = ", offset.x,offset.y
			blockGrid(block.x)(block.y)(offset.x,offset.y)
	}

	def getCellDomain(lattice:Lattice2D,domain:Box2D) = {
		val layer = new Array[Array[Cell]](domain.x1-domain.x0,domain.y1-domain.y0)
		for (iX <- domain.x0 to domain.x1; iY <- domain.y0 to domain.y1) {
			layer(iX - domain.x0)(iY - domain.y0) = lattice(iX,iY)
		}
		layer
	}

	def copyFromFlatBulkToEnvelope(lattice:Lattice2D,iNx:Int,iNy:Int,domain:Box2D,dir:Int,orient:Int) = {
			// The part invol
			val layer = getCellDomain(lattice, domain)
			val next = new Array[Int](D.d)
			next(dir) = orient
			val nextX = (iNx + next(0) + nBlockX) % nBlockX
			val nextY = (iNy + next(1) + nBlockY) % nBlockY

			val nextLattice = blockGrid(nextX)(nextY)
			val nextDomain = multiUtils.getNextFlatDomain(nextLattice, dir, orient)
			//print "nextDomain = ", nextDomain.x0,nextDomain.x1,nextDomain.y0,nextDomain.y1
			//print domain.x0,domain.x1,domain.y0,domain.y1, nextDomain.x0,nextDomain.x1,nextDomain.y0,nextDomain.y1
			multiUtils.copyCellsDomain(nextLattice,nextDomain,layer)
  }

	def copyFromCornerBulkToEnvelope(lattice:Lattice2D,iNx:Int,iNy:Int,
                                   domain:Box2D,xNormal:Int,yNormal:Int) = {
    // The part invol
    val layer = getCellDomain(lattice, domain)
    val next = new Dot2D(xNormal, yNormal)
    val nextX = (iNx + next.x + nBlockX) % nBlockX
    val nextY = (iNy + next.y + nBlockY) % nBlockY

    val nextLattice = blockGrid(nextX)(nextY)
    val nextDomain = multiUtils.getNextCornerDomain(nextLattice, xNormal, yNormal)
    multiUtils.copyCellsDomain(nextLattice,nextDomain,layer)
  }

	def fromBulkToNeighborEnvelope() = {
		for (iNx <- 0 until nBlockX) {
			for (iNy <- 0 until nBlockY) {
				val lattice = blockGrid(iNx)(iNy)
				val zero = D.vicinity
				val delta = D.vicinity-1

				val nx = lattice.nX-1
				val ny = lattice.nY-1

				//flat wallf domains
				val left = new Box2D(zero,zero+delta,zero,ny-zero)
        val right = new Box2D(nx-zero,nx-zero+delta,zero,ny-zero)
        val bottom = new Box2D(zero,nx-zero,zero,zero+delta)
        val top = new Box2D(zero,nx-zero,ny-zero,ny-zero+delta)

				//corner domains (lt -> left-top, rt -> right-top, lb -> left-bottom, rb -> right-bottom)
        val lt = new Box2D(zero,         zero+delta,ny-zero+delta,ny-zero)
        val rt = new Box2D(nx-zero+delta,nx-zero,   ny-zero+delta,ny-zero)
        val lb = new Box2D(zero,         zero+delta,zero,         zero+delta)
        val rb = new Box2D(nx-zero+delta,nx-zero,   zero,         zero+delta)

				// on the bulk interface, the cells must be copied to the envelope
				// of the neighbor lattice. Here we do tha flat part
				// we are still left with the corners.
				//print "walls : ", "num_x = ", iNx, "num_y = ", iNy, ", x_size = ", nxSizes[iNx], ", y_size = ", nySizes[iNy]
				//print "left = ",left.x0,left.x1,left.y0,left.y1
				copyFromFlatBulkToEnvelope(lattice,iNx,iNy,left,  0,-1)
				//print "right = ",right.x0,right.x1,right.y0,right.y1
				copyFromFlatBulkToEnvelope(lattice,iNx,iNy,right, 0,+1)
				//print "bottom = ", bottom.x0,bottom.x1,bottom.y0,bottom.y1
				copyFromFlatBulkToEnvelope(lattice,iNx,iNy,bottom,1,-1)
				//print "top = ",top.x0,top.x1,top.y0,top.y1
				copyFromFlatBulkToEnvelope(lattice,iNx,iNy,top,   1,+1)
				
				copyFromCornerBulkToEnvelope(lattice,iNx,iNy,lt, -1,+1 )
				copyFromCornerBulkToEnvelope(lattice,iNx,iNy,lb, -1,-1 )
				copyFromCornerBulkToEnvelope(lattice,iNx,iNy,rt, +1,+1 )
				copyFromCornerBulkToEnvelope(lattice,iNx,iNy,rb, +1,-1 )
			}
		}
	}

  def initialize() = fromBulkToNeighborEnvelope

	def collide() = {
		for (iNx <- 0 until nBlockX;iNy <- 0 until nBlockY) blockGrid(iNx)(iNy).collide
	}

	def stream() = {
			for (iNx <- 0 until nBlockX; iNy <- 0 until nBlockY) blockGrid(iNx)(iNy).stream
	}

	def dataProcess() = {
			for (inX <- 0 until nBlockX; inY <- 0 until nBlockY) blockGrid(inX)(inY).dataProcess
	}

	def collideAndStream() = {
		collide()
		stream()

		dataProcess()

		fromBulkToNeighborEnvelope()
	}
}
