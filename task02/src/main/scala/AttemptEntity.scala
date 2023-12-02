import cats.syntax.monoid._
import cats.Monoid

final case class AttemptEntity(colors: AttemptEntity.Colors) {
  lazy val reds: Long = colors.getOrElse(Color.Red, 0)
  lazy val greens: Long = colors.getOrElse(Color.Green, 0)
  lazy val blues: Long = colors.getOrElse(Color.Blue, 0)
}

object AttemptEntity {
  type Colors = Map[Color, Long]

  def isPossible(attempt: AttemptEntity, limits: Colors): Boolean =
    attempt.reds <= limits.getOrElse[Long](Color.Red, 0) &&
      attempt.greens <= limits.getOrElse[Long](Color.Green, 0) &&
      attempt.blues <= limits.getOrElse[Long](Color.Blue, 0)

  def fromStr(str: String): Seq[AttemptEntity] = str.split(';').flatMap(attempt => getAttempt(attempt.trim))

  private def getAttempt(str: String): Option[AttemptEntity] = {
    val result = getColors(str).foldLeft(Monoid[Colors].empty) {
      case (acc, (color, number)) => acc |+| Map(color -> number)
    }
    Option.when(result != Monoid[Colors].empty)(AttemptEntity(result))
  }

  private def getColors(str: String): Seq[(Color, Long)] = str.split(',').flatMap(color => getColor(color.trim))

  private def getColor(str: String): Option[(Color, Long)] = {
    val split = str.split(' ')
    for {
      count <- split.headOption.flatMap(_.toLongOption)
      color <- split.lastOption.flatMap(Color.fromStr)
    } yield (color, count)
  }
}