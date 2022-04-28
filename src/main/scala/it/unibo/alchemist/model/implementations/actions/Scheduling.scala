package it.unibo.alchemist.model.implementations.actions
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import org.apache.commons.math3.random.RandomGenerator
trait Scheduling {
  def eval(context: Context)(logic: => EXPORT): Unit
}

object ExecuteAlways extends Scheduling {
  override def eval(context: Context)(
      logic: => EXPORT
  ): Unit = logic
}

class RandomlyNotExecute() extends Scheduling {
  override def eval(context: Context)(
      logic: => EXPORT
  ): Unit = {
    val random = context.sense[RandomGenerator](LSNS_ALCHEMIST_RANDOM).get
    if (random.nextBoolean()) {
      logic
    }
  }
}
