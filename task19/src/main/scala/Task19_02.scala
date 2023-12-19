import model._

object Task19_02 extends Task19 {
  override def fileName: String = "19_input.txt"

  override def calculate(instructions: Instructions, resource: Seq[Resource]): BigInt =
    instructions.findAcceptancesFor(Resource.interval).collect {
      case (resource, Workflow.Accept) => resource.getItems.collect { case Item.Interval(from, to) => BigInt(to - from + 1) }.product
    }.sum
}
