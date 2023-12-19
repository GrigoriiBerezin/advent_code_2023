package model

import cats.syntax.option._

sealed trait Rule {
  def getAcceptable(resource: Resource): (Option[Resource], Option[Resource])
}

object Rule {
  def build(line: String): Rule = {
    def inner(rulesRaw: List[String]): Rule = rulesRaw match {
      case s"$key>$value:$workflow" :: leftRules => value.toIntOption
        .map(limit => Condition(key, Ratio.GreaterThan, limit, Workflow.withName(workflow), inner(leftRules)))
        .getOrElse(Default(Workflow.withName(workflow)))
      case s"$key<$value:$workflow" :: leftRules => value.toIntOption
        .map(limit => Condition(key, Ratio.LessThan, limit, Workflow.withName(workflow), inner(leftRules)))
        .getOrElse(Default(Workflow.withName(workflow)))
      case s"$workflow" :: Nil => Default(Workflow.withName(workflow))
      case _ => Default(Workflow.Reject)
    }

    inner(line.split(',').toList)
  }

  final case class Condition(key: String, ratio: Ratio, limit: Int, to: Workflow, elseTo: Rule) extends Rule {
    override def getAcceptable(resource: Resource): (Option[Resource], Option[Resource]) = (resource.byKey(key), ratio) match {
      case (Some(Item.Value(value)), _) =>
        (Option.when(ratio.operate(limit)(value))(resource),
          Option.when(!ratio.operate(limit)(value))(resource))
      case (Some(Item.Interval(from, to)), Ratio.LessThan) =>
        (Option.when(ratio.operate(limit)(from))(resource.update(key, Item.Interval(from, Math.min(limit - 1, to)))),
          Option.when(ratio.operate(to)(limit))(resource.update(key, Item.Interval(limit, to))))
      case (Some(Item.Interval(from, to)), Ratio.GreaterThan) =>
        (Option.when(ratio.operate(limit)(to))(resource.update(key, Item.Interval(Math.max(limit + 1, from), to))),
          Option.when(ratio.operate(from)(limit))(resource.update(key, Item.Interval(from, limit))))
      case _ => (none[Resource], resource.some)
    }
  }
  final case class Default(to: Workflow) extends Rule {
    override def getAcceptable(resource: Resource): (Option[Resource], Option[Resource]) = (resource.some, none[Resource])
  }
}
