package it.unibo.rl.model

import it.unibo.rl.model.QRL.QLParameter
import upickle.default._
import scala.util.Random

trait QRLImpl[S, A] extends QRL[S, A] with Serializable {

  /** A Map-based implementation, with defaults for terminal and unexplored states
    * @param actions
    * @param v0
    * @param terminal
    * @param terminalValue
    */
  case class QFunction(
      override val actions: Set[A],
      v0: R = 0.0,
      @transient terminal: S => Boolean = (s: S) => false,
      terminalValue: Double = 0.0
  ) extends Q
      with Serializable {
    val map: collection.mutable.Map[(S, A), R] = collection.mutable.Map()
    override def apply(s: S, a: A): R = if (terminal(s)) terminalValue else map.getOrElse(s -> a, v0)
    override def update(s: S, a: A, v: Double): Q = { map += ((s -> a) -> v); this }
    override def toString: String = map.toString
  }

  def storeQ(q: QFunction)(implicit s: ReadWriter[A], a: ReadWriter[S]): String = write(q.map)

  def loadFromMap(base: QFunction, string: String)(implicit s: ReadWriter[A], a: ReadWriter[S]): Unit =
    read[Map[(S, A), R]](string).foreach { case ((s, a), v) => base.update(s, a, v) }

  case class RealtimeQLearning(gamma: Double, q0: Q, parameter: QLParameter) {
    private var state: Option[S] = None
    private var action: Option[A] = None

    def setState(s: S): Unit = state = Some(s)
    def takeAction(a: A): Unit = action = Some(a)
    def takeEpsGreedyAction(qf: Q)(implicit rand: Random): A = {
      val epsilon = parameter.epsilon
      //println(s"epsilon value: $epsilon at time $time, st: ${state.get}")
      val a = qf.explorationPolicy(epsilon)(rand)(state.get)
      takeAction(a)
      a
    }

    def takeGreedyAction(qf: Q): A = qf.greedyPolicy(state.get)
    def observeEnvAndUpdateQ(qf: Q, newState: S, reward: R): Q = try {
      val alpha = parameter.alpha
      val beta = parameter.beta
      //println(s"alpha value $alpha at time $time")
      (for {
        s <- state
        a <- action
      } yield {
        // By wikipedia (weighted average)
        // val vr = (1 - alpha) * qf(s, a) + alpha * (reward + gamma * qf.optimalVFunction(newState))
        // By book
        val update = reward + gamma * qf.optimalVFunction(newState) - qf(s, a)
        val vr = qf(s, a) + update * (if (update > 0) {
                                        alpha
                                      } else {
                                        beta
                                      })
        //println(s"State: $s, Action: $a, Value: $vr, Alpha: $alpha, Pair: ${qf(s,a)}, optimal: ${qf.optimalVFunction(newState)} ")
        //println(s"Value: $vr, optimal: ${qf.optimalVFunction(newState)} ")
        qf.update(s, a, vr)
      }).getOrElse(q0)
    } finally setState(newState)
  }
}
