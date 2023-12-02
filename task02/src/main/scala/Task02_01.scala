import zio._

object Task02_01 extends ZIOAppDefault with ReadFileSuite with Task02 {
  override protected def calculate(games: Seq[GameEntity]): UIO[Long] = ZIO.succeed(games.collect {
    case game if GameEntity.isPossible(game, limits) => game.id
  }.sum)

  private val limits: Map[Color, Long] = Map(Color.Red -> 12, Color.Green -> 13, Color.Blue -> 14)
}
