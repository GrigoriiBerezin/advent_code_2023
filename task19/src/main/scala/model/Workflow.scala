package model

sealed trait Workflow {
  def name: String
}

object Workflow {
  def withName(name: String): Workflow =
    Seq(Start, Reject, Accept).find(_.name == name).getOrElse(ActiveWorkflow(name))

  final case class ActiveWorkflow(name: String) extends Workflow

  final case object Start extends Workflow {
    override def name: String = "in"
  }

  final case object Reject extends Workflow {
    override def name: String = "R"
  }

  final case object Accept extends Workflow {
    override def name: String = "A"
  }
}
