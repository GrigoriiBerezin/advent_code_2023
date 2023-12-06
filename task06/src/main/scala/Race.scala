import scala.annotation.tailrec

final case class Race(time: Long, distance: Long) {
  lazy val winTimes: Long = {
    @tailrec
    def firstIntoBrake(curTime: Long): Long =
      if (curTime > time) curTime
      else {
        val lastTime = time - curTime
        if (lastTime * curTime > distance) curTime else firstIntoBrake(curTime + 1)
      }
    @tailrec
    def lastIntoBrake(curTime: Long): Long =
      if (curTime < 0) curTime
      else {
        val lastTime = time - curTime
        if (lastTime * curTime > distance) curTime else lastIntoBrake(curTime - 1)
      }

    val start = firstIntoBrake(0)
    val end = lastIntoBrake(time)
    end - start + 1
  }
}
