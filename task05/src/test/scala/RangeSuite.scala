import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RangeSuite extends AnyFlatSpec with Matchers {

  "findDestinationResourceWithRange" should "select all ranges" in {
    val seed = ResourceWithRange(1, 100, DestinationEnum.Seed)
    val properties = Seq(
      Property(14, 15, 6),
      Property(23, 25, 6)
    )
    val map = ResourceMap(DestinationEnum.Seed, DestinationEnum.Temperature, properties)

    val expectedRanges = Seq(
      ResourceWithRange(1, 14, DestinationEnum.Temperature),
      ResourceWithRange(14, 19, DestinationEnum.Temperature),
      ResourceWithRange(21, 24, DestinationEnum.Temperature),
      ResourceWithRange(23, 28, DestinationEnum.Temperature),
      ResourceWithRange(31, 100, DestinationEnum.Temperature)
    )

    map.findDestinationResourceWithRange(seed) should contain theSameElementsAs expectedRanges
  }

  it should "select range from out of range" in {
    val seed = ResourceWithRange(32, 40, DestinationEnum.Soil)
    val properties = Seq(
      Property(15, 14, 5),
      Property(17, 21, 5)
    )
    val map = ResourceMap(DestinationEnum.Soil, DestinationEnum.Light, properties)

    val expectedRanges = Seq(
      ResourceWithRange(32, 40, DestinationEnum.Light)
    )

    map.findDestinationResourceWithRange(seed) should contain theSameElementsAs expectedRanges
  }

  it should "select range from out of range between ranges" in {
    val seed = ResourceWithRange(15, 20, DestinationEnum.Soil)
    val properties = Seq(
      Property(11, 10, 5),
      Property(17, 21, 5)
    )
    val map = ResourceMap(DestinationEnum.Soil, DestinationEnum.Light, properties)

    val expectedRanges = Seq(
      ResourceWithRange(15, 20, DestinationEnum.Light)
    )

    map.findDestinationResourceWithRange(seed) should contain theSameElementsAs expectedRanges
  }

  it should "select range from under of first range" in {
    val seed = ResourceWithRange(1, 5, DestinationEnum.Soil)
    val properties = Seq(
      Property(11, 10, 5),
      Property(17, 21, 5)
    )
    val map = ResourceMap(DestinationEnum.Soil, DestinationEnum.Light, properties)

    val expectedRanges = Seq(
      ResourceWithRange(1, 5, DestinationEnum.Light)
    )

    map.findDestinationResourceWithRange(seed) should contain theSameElementsAs expectedRanges
  }

}
