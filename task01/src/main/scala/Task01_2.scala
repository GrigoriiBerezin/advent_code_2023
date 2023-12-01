import zio._

object Task01_2 extends ZIOAppDefault with ReadFileSuite with CalculateBase {

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    lines <- getLines("01_input.txt")
    result <- calculate(lines)
    _ <- Console.printLine(result)
  } yield ExitCode.success

  override protected def getNumber(line: String): Option[Long] = {
    val foundNumbers = numbers.flatMap(number => Seq((line.lastIndexOf(number), number), (line.indexOf(number), number)))
      .filter { case (index, _) => index != -1 }
      .map { case (index, number) => (index, converter(number)) }
      .sortBy { case (index, _) => index }
    for {
      (_, first) <- foundNumbers.headOption
      (_, last) <- foundNumbers.lastOption
    } yield first * 10 + last
  }

  private val numbers: Seq[String] = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine") ++
    (1 to 9).map(_.toString)

  private val converter: PartialFunction[String, Long] = {
    case "one" => 1
    case "two" => 2
    case "three" => 3
    case "four" => 4
    case "five" => 5
    case "six" => 6
    case "seven" => 7
    case "eight" => 8
    case "nine" => 9
    case other => other.toLong
  }
}
