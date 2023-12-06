import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RaceSpec extends AnyFlatSpec with Matchers {
  "winTimes" should "contain the correct win times in race" in {
    val race = Race(7, 9)
    race.winTimes shouldEqual 4
  }
}
