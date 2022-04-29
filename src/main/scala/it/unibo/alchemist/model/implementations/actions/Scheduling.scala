package it.unibo.alchemist.model.implementations.actions
import it.unibo.alchemist.model.CachedInterpreter
import it.unibo.alchemist.model.implementations.nodes.NodeManager
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.casestudy.RLAgent
import it.unibo.casestudy.RLAgent.{EnergySaving, FullSpeed, Normal, OutputDirection, RisingDown, RisingUp, Same, State}
import it.unibo.rl.model.QRLImpl
import it.unibo.utils.RichDouble._
import org.apache.commons.math3.random.RandomGenerator

import scala.collection.mutable
trait Scheduling {
  def eval(context: Context, node: NodeManager)(logic: => EXPORT): Unit
}

object ExecuteAlways extends Scheduling {
  override def eval(context: Context, node: NodeManager)(
      logic: => EXPORT
  ): Unit = logic
}

class RandomlyNotExecute() extends Scheduling {
  override def eval(context: Context, node: NodeManager)(
      logic: => EXPORT
  ): Unit = {
    val random = context.sense[RandomGenerator](LSNS_ALCHEMIST_RANDOM).get
    if (random.nextBoolean()) {
      logic
    }
  }
}

class QBasedScheduling(where: String) extends Scheduling {
  import QBasedScheduling._
  val qFunction: QRLFamily.QFunction = CachedInterpreter.cache(where) {
    val load = os.read(os.Path(os.pwd.toString() + "/" + where))
    val qFunction = QRLFamily.QFunction(Set(EnergySaving, FullSpeed, Normal))
    QRLFamily.loadFromMap(qFunction, load)
    qFunction
  }
  val schedulingCache = mutable.Map[String, SchedulingLogic]()
  protected var reinforcementLearningProcess: QRLFamily.RealtimeQLearning =
    QRLFamily.RealtimeQLearning(0, qFunction, null)
  override def eval(context: Context, node: NodeManager)(
      logic: => EXPORT
  ): Unit =
    schedulingCache.getOrElseUpdate(node.id, new SchedulingLogic()).eval(context, node)(logic)
  class SchedulingLogic() {
    private var skip = 0
    protected var state: State = State(FullSpeed, Seq.empty[OutputDirection])
    protected var oldValue: Double = Double.PositiveInfinity
    protected val temporalWindow = 5
    def eval(context: Context, node: NodeManager)(
        logic: => EXPORT
    ): Unit = {
      node.put("skip status", skip)
      if (skip == 0) {
        val output = logic
        val result = output.root[Double]()
        val direction = outputTemporalDirection(result)
        val currentHistory = state.history
        val action = qFunction.greedyPolicy(state)
        node.put("skip", action.skip)
        node.put("history", currentHistory)
        node.put("elements", (direction +: currentHistory))
        node.put("oldValue", oldValue)
        node.put("current", result)
        node.put("same", oldValue ~= result)
        node.put("direction = ", outputTemporalDirection(result))
        node.put("direction 1 = ", direction)
        node.put("action", action)
        node.put("result", result)
        state = State(action, (direction +: currentHistory).take(temporalWindow))
        oldValue = result
        skip = action.skip
      } else {
        skip -= 1
      }
    }

    private def outputTemporalDirection(current: Double): OutputDirection = if (current ~= oldValue) {
      Same
    } else if (current > oldValue) {
      RisingUp
    } else {
      RisingDown
    }
  }
}

object QBasedScheduling {
  val QRLFamily: QRLImpl[RLAgent.State, RLAgent.WeakUpAction] = new QRLImpl[RLAgent.State, RLAgent.WeakUpAction] {}
}
