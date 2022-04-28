package it.unibo.scafi
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
class BaseProgram extends AggregateProgram with StandardSensors {
  override def main(): Any = rep(Double.PositiveInfinity) { g =>
    mux(mid == 10)(0.0)(minHoodPlus(nbr(g) + nbrRange()))
  }
}
