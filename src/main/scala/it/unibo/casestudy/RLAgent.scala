package it.unibo.casestudy

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps
import upickle.default._

object RLAgent {
  sealed abstract class WeakUpAction(val next: FiniteDuration, val skip: Int)
  case object EnergySaving extends WeakUpAction(1 seconds, 10)
  case object FullSpeed extends WeakUpAction(100 milliseconds, 0)
  case object Normal extends WeakUpAction(200 milliseconds, 1)

  sealed trait OutputDirection
  case object Same extends OutputDirection
  case object RisingUp extends OutputDirection
  case object RisingDown extends OutputDirection

  case class State(currentSetting: WeakUpAction, history: Seq[OutputDirection])

  implicit val stateRW: ReadWriter[State] = macroRW[State]
  implicit val outputRW: ReadWriter[OutputDirection] = macroRW[OutputDirection]
  implicit val actionRW: ReadWriter[WeakUpAction] = macroRW[WeakUpAction]
}
