package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
class BaseProgram extends AggregateProgram with StandardSensors with ScafiAlchemistSupport with BlockG {
  override def main(): Any = classicGradient(source)

  def source = node.has("source")
}
