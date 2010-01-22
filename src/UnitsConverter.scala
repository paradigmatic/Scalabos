package lb.units

class UnitsConverter[T <: Descriptor](D:T, val Re:Double, val physVel:Double,lbVel:Double,
                                     physLength:Double,lbLength:Int,lx:Double,ly:Double) {
  
      def deltaX() : Double = physLength / lbLength

      def deltaT() : Double = lbVel / physVel * deltaX

      def nCell(l:Double) : Int = (l / deltaX + 0.5).toInt

      def nX() : Int =  nCell(physLength*lx)+1
      
      def nY() : Int =  nCell(physLength*ly)+1

      def physNu() : Double = physVel*physLength / Re
      
      def lbNu() : Double = lbVel*lbLength / Re

      def omega(): Double = 1.0 / (lbNu * D.invCs2 + 0.5)         
}
