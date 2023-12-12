case class Galaxy(stars: Seq[Star], emptyGalaxyRows: Seq[Int], emptyGalaxyColumns: Seq[Int], valueOfEmptyGalaxy: Int) {
  def distanceBetweenStars(star1: Star, star2: Star): Int = {
    val minX = Math.min(star1.x, star2.x)
    val maxX = Math.max(star1.x, star2.x)
    val minY = Math.min(star1.y, star2.y)
    val maxY = Math.max(star1.y, star2.y)
    maxX - minX +
      maxY - minY +
      emptyGalaxyColumns.count(x => x > minX && x < maxX) * Math.max(valueOfEmptyGalaxy - 1, 1) +
      emptyGalaxyRows.count(y => y > minY && y < maxY) * Math.max(valueOfEmptyGalaxy - 1, 1)
  }
}

object Galaxy {
  def convertToNonEmptyGalaxy(galaxy: Galaxy): Galaxy = {
    val stars = galaxy.stars.map(star => Star(
      star.x + galaxy.emptyGalaxyColumns.count(_ < star.x) * galaxy.valueOfEmptyGalaxy,
      star.y + galaxy.emptyGalaxyRows.count(_ < star.y) * galaxy.valueOfEmptyGalaxy
    ))
    Galaxy(stars, Seq.empty, Seq.empty, 1)
  }

  def build(lines: Seq[String], valueOfEmptyGalaxy: Int): Galaxy = {
    val stars = lines.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.collect {
        case (Star.identifier, x) => Star(x, y)
      }
    }
    lines match {
      case Seq(head, _@_*) =>
        val emptyGalaxyRows = lines.indices.filterNot(y => stars.exists(_.y == y))
        val emptyGalaxyColumns = head.indices.filterNot(x => stars.exists(_.x == x))
        Galaxy(stars, emptyGalaxyRows, emptyGalaxyColumns, valueOfEmptyGalaxy)
      case _ => Galaxy(stars, Seq.empty, Seq.empty, valueOfEmptyGalaxy)
    }
  }
}
