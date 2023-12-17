object Task14_01 extends Task14 {
  override def fileName: String = "14_input.txt"

  override def rotateMap: RockMap => RockMap = _.moveRocks(Direction.North)
}
