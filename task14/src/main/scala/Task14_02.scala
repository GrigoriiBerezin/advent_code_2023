object Task14_02 extends Task14 {
  override def fileName: String = "14_input.txt"

  override def rotateMap: RockMap => RockMap = map => {
    val (newMap, cycleResult) = map.moveUntilCycle
    val leftIters = (1000000000 - cycleResult.iteration) % cycleResult.size
    (1 to leftIters).foldLeft(newMap)((acc, _) => acc.moveCycle)
  }
}
