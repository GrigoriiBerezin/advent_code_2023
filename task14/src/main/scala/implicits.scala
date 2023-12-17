object implicits {
  implicit class FieldsOps(val value: Vector[Vector[Element]]) extends AnyVal {
    def switchRock(from: Coordinates, to: Coordinates): Vector[Vector[Element]] = {
      val removedFrom = value.updated(from.y, value(from.y).updated(from.x, Element.Empty))
      val addedTo = removedFrom.updated(to.y, removedFrom(to.y).updated(to.x, Element.MovableRock))
      addedTo
    }
  }
}
