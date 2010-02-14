package lb.util

class Box2D(val x0:Int, val x1:Int, val y0:Int, val y1:Int) {
	def resize(size:Int) = new Box2D(x0-size,x1+size,y0-size,y1+size)
	
	def shiftX(offset:Int) = new Box2D(x0+offset,x1+offset,y0,y1)
	def shiftY(offset:Int) = new Box2D(x0,x1,y0+offset,y1+offset)
}

class Dot2D(val x:Int, val y:Int) {
	def shiftX(offset:Int) = new Dot2D(x+offset,y)
	def shiftY(offset:Int) = new Dot2D(x,y+offset)
	
	def shift(off:Dot2D) = new Dot2D(x+off.x,y+off.y)
}
