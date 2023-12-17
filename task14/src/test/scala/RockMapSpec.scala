import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.show._

import scala.io.Source

class RockMapSpec extends AnyFlatSpec with Matchers {

  private val lines: Seq[String] = Source.fromResource("test_input.txt").getLines().toSeq
  private val rockMap: RockMap = RockMap.build(lines)

  "getCoordinatesOf" should "return sequence of movable rock coordinates" in {
    val expectedCoordinates = Seq(
      Coordinates(5, 0),
      Coordinates(4, 1),
      Coordinates(9, 1),
      Coordinates(5, 2),
      Coordinates(6, 2),
      Coordinates(3, 3),
      Coordinates(8, 4),
      Coordinates(2, 5),
      Coordinates(7, 5),
      Coordinates(9, 5),
      Coordinates(5, 6),
      Coordinates(0, 8),
      Coordinates(5, 8),
      Coordinates(6, 8),
      Coordinates(7, 8),
      Coordinates(0, 9),
      Coordinates(5, 9),
    )

    rockMap.getCoordinatesOf(Element.StableRock) should contain theSameElementsAs  expectedCoordinates
  }

  "moveRock" should "return map with all movable rocks moved" in {
    val lines: Seq[String] = Source.fromResource("north_moved.txt").getLines().toSeq
    val movedMap: RockMap = RockMap.build(lines)

    rockMap.moveRocks(Direction.North) shouldEqual movedMap
  }

  "moveCycle" should "return correct map after 3 cycles" in {
    val lines: Seq[String] = Source.fromResource("3_cycles.txt").getLines().toSeq
    val cycledMap = RockMap.build(lines)

    (1 to 3).foldLeft(rockMap)((acc, _) => acc.moveCycle) shouldEqual cycledMap
  }
}
