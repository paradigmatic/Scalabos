package lb

trait Descriptor {

  def Q(): Int
  def D(): Int
  def C(): Array[Array[Int]]
  def T(): Array[Double]
  def invCs2(): Double
  def cs2 = 1.0 / invCs2

}
