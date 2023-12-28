final case class VectorMath(x: Int, y: Int, z: Int) {
  def *(moves: Int): VectorMath = VectorMath(x * moves, y * moves, z * moves)
  def *(vector: VectorMath): VectorMath = VectorMath(y * vector.z - z * vector.y, x * vector.z - z * vector.x, x * vector.y - y * vector.x)
}

