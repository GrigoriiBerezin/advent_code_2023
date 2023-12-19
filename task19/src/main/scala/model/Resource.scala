package model

case class Resource(private val map: Map[String, Item]) {
  def update(key: String, value: Item): Resource = copy(map = map.updated(key, value))
  def byKey(key: String): Option[Item] = map.get(key)

  lazy val getItems: Seq[Item] = map.values.toList
}

object Resource {
  lazy val interval: Resource = Resource(Seq("x", "m", "a", "s").map(_ -> Item.Interval(1, 4000)).toMap)

  def build(line: String): Option[Resource] = line match {
    case s"{$valuesStr}" => Some(
      Resource(valuesStr.split(',').flatMap {
        case s"$key=$value" => Item(value).map(key -> _)
        case _ => None
      }.toMap)
    )
    case _ => None
  }
}
