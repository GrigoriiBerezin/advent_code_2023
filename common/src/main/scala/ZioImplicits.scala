import zio._

object ZioImplicits {
  implicit class ZioOps[A](func: => A) {
    def succeed: UIO[A] = ZIO.succeed(func)
    def attempt: Task[A] = ZIO.attempt(func)
  }
}
