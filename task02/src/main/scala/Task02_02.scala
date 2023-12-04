import AttemptEntity.Colors
import ZioImplicits.ZioOps
import cats.syntax.monoid._
import cats.Monoid
import cats.kernel.CommutativeGroup
import zio._

object Task02_02 extends ZIOAppDefault with ReadFileSuite with Task02 {
  private implicit val longCommutativeGroup: CommutativeGroup[Long] = new CommutativeGroup[Long] {
    override def inverse(a: Long): Long = -a

    override def empty: Long = 0

    override def combine(x: Long, y: Long): Long = Math.max(x, y)
  }

  override protected def calculate(games: Seq[GameEntity]): UIO[Long] = games.map(getFewLimit).sum.succeed

  private def getFewLimit(game: GameEntity): Long =
    game.attempts.map(_.colors)
      .foldLeft(Monoid[Colors].empty)(_ |+| _)
      .values
      .product
}
