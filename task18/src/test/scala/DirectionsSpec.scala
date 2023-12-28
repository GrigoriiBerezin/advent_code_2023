import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class DirectionsSpec extends AnyFlatSpec with Matchers {
  private val directionsWithInto = Table(
    ("from", "to"),
    (Direction.Right, Direction.Down),
    (Direction.Down, Direction.Left),
    (Direction.Left, Direction.Up),
    (Direction.Up, Direction.Right)
  )

  private val directionsWithOut = Table(
    ("from", "to"),
    (Direction.Right, Direction.Up),
    (Direction.Down, Direction.Right),
    (Direction.Left, Direction.Down),
    (Direction.Up, Direction.Left)
  )

  "Directions multiply" should "return correct direction with into direction" in {
    forAll(directionsWithInto) { (from: Direction, to: Direction) =>
      from.rotateBy(Direction.Into) should contain(to)
    }
  }

  "Directions multiply" should "return correct direction with out direction" in {
    forAll(directionsWithOut) { (from: Direction, to: Direction) =>
      from.rotateBy(Direction.Out) should contain(to)
    }
  }
}
