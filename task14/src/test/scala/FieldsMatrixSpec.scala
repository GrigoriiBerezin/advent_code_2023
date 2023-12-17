import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import implicits._

class FieldsMatrixSpec extends AnyFlatSpec with Matchers {

  "switchRocks" should "switch elements in matrix" in {
    val strFields =
      """O..#.
        |..#..
        |""".stripMargin
    val fields = strFields.split(System.lineSeparator()).toVector.map(_.map(Element.withName).toVector)

    val expectedStrFields =
      """O.O#.
        |.....
        |""".stripMargin
    val expectedFields = expectedStrFields.split(System.lineSeparator()).toVector.map(_.map(Element.withName).toVector)

    fields.switchRock(Coordinates(2, 1), Coordinates(2, 0)) shouldEqual expectedFields
  }

}
