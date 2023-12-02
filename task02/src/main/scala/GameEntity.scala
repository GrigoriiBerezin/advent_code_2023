import AttemptEntity.Colors
import cats.syntax.option._

final case class GameEntity(id: Long, attempts: Seq[AttemptEntity])

object GameEntity {
  def isPossible(game: GameEntity, limits: Colors): Boolean = game.attempts.forall(attempt => AttemptEntity.isPossible(attempt, limits))

  def fromStr(str: String): Option[GameEntity] = {
    val split = str.split(':')
    for {
      game <- split.headOption.flatMap(getGame)
      attempts <- split.lastOption.map(AttemptEntity.fromStr)
    } yield GameEntity(game, attempts)
  }

  private def getGame(str: String): Option[Long] = {
    if (str.startsWith(gameIdentity)) str.replaceFirst(gameIdentity + " ", "").toLongOption
    else none[Long]
  }

  private val gameIdentity: String = "Game"
}