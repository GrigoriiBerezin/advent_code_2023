import model.{Instructions, Item, Resource, Workflow}

object Task19_01 extends Task19 {
  override def fileName: String = "19_input.txt"

  override def calculate(instructions: Instructions, resources: Seq[Resource]): BigInt =
    resources.flatMap(r => instructions.findAcceptancesFor(r)).collect {
      case (resource, Workflow.Accept) => resource.getItems.collect { case Item.Value(v) => v }.sum
    }.sum
}
