package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
class BaseProgram extends AggregateProgram with StandardSensors with ScafiAlchemistSupport {
  override def main(): Any = rep(Double.PositiveInfinity) { g =>
    mux(source)(0.0)(minHoodPlus(nbr(g) + nbrRange()))
  }

  def source = node.has("source")
}
