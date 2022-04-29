package it.unibo.rl.model

import scala.util.Random

/** Type family that contains the definition of the abstractions needs for a Reinforcement Learning environment based on
  * Q
  * @tparam S
  *   the state type
  * @tparam A
  *   the action type
  */
trait QRL[S, A] {
  type R = Double // Reward
  type P = Double // Probability
  type Policy = S => A
  type VFunction = S => R
  /** Q is an updatable, table-oriented state-action value function, to optimise selection over certain actions */
  trait Q extends ((S, A) => R) {
    /** @return The discrete action set that is contained in the table */
    def actions: Set[A]

    def update(state: S, action: A, value: R): Q

    /** @return Create a greedy policy (i.e. the one that chooses the action with the highest value for a give state) */
    def greedyPolicy: Policy = s => actions.maxBy(this(s, _))

    /** @return
      *   Create an epsilon-greedy policy (i.e. the one that chooses the action with the highest value for a give state
      *   with 1 - epsilon probability, a random action otherwise)
      */
    def epsilonGreedyPolicy(epsilon: P)(implicit rand: Random): Policy = explorationPolicy(epsilon)

    def explorationPolicy(f: P)(implicit rand: Random): Policy = {
      case _ if Stochastics.drawFiltered(_ < f) => Stochastics.uniformDraw(actions)
      case s => greedyPolicy(s)
    }

    /** @return the V function from the Q table */
    def optimalVFunction: VFunction = s => actions.map(this(s, _)).max
  }
}

object QRL {
  /** The parameters needed for create a Q learning */
  trait QLParameter {
    def epsilon: Double
    def alpha: Double
    def beta: Double
  }

  case class StaticParameters(epsilon: Double, alpha: Double, beta: Double) extends QLParameter
}
