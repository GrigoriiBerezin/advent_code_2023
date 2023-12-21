package model

case class Instructions(private val map: Map[Workflow, Rule]) {
  def findAcceptancesFor(resource: Resource): Seq[(Resource, Workflow)] = {
    def inner(rule: Rule, resource: Resource): Seq[(Resource, Workflow)] = rule match {
      case Rule.Condition(_, _, _, to: Workflow.ActiveWorkflow, elseTo) =>
        val (accepted, notAccepted) = rule.getAcceptable(resource)
        val left = for {
          accept <- accepted
          nextRule <- map.get(to)
        } yield inner(nextRule, accept)
        left.getOrElse(List.empty) ++ notAccepted.map(inner(elseTo, _)).getOrElse(List.empty)
      case Rule.Condition(_, _, _, to, elseTo) =>
        val (accepted, notAccepted) = rule.getAcceptable(resource)
        accepted.toList.map((_, to)) ++ notAccepted.map(inner(elseTo, _)).getOrElse(List.empty)
      case Rule.Default(to) =>
        val (accepted, _) = rule.getAcceptable(resource)
        to match {
          case _: Workflow.ActiveWorkflow =>
            (for {
              accept <- accepted
              nextRule <- map.get(to)
            } yield inner(nextRule, accept)).getOrElse(List.empty)
          case _ => accepted.map((_, to)).toList
        }
    }

    map.get(Workflow.Start).map(inner(_, resource)).getOrElse(Seq.empty)
  }
}

object Instructions {
  def build(lines: Seq[String]): Instructions = Instructions(lines.collect {
    case s"$workflow{$rule}" => Workflow.withName(workflow) -> Rule.build(rule)
  }.toMap)
}
