/*
 * Copyright (C) 2010-2019, Danilo Pianini and contributors listed in the main project's alchemist/build.gradle file.
 *
 * This file is part of Alchemist, and is distributed under the terms of the
 * GNU General Public License, with a linking exception,
 * as described in the file LICENSE in the Alchemist distribution's top directory.
 */
package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.interfaces.{Time => AlchemistTime, _}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.{ContextImpl, _}
import it.unibo.alchemist.scala.PimpMyAlchemist._
import it.unibo.scafi.space.Point3D
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.FastMath
import org.kaikikm.threadresloader.ResourceLoader

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}

sealed class RunScafiProgram[T, P <: Position[P]](
    environment: Environment[T, P],
    node: Node[T],
    reaction: Reaction[T],
    rng: RandomGenerator,
    programName: String,
    retentionTime: Double,
    scheduling: Scheduling
) extends AbstractLocalAction[T](node) {

  def this(
      environment: Environment[T, P],
      node: Node[T],
      reaction: Reaction[T],
      rng: RandomGenerator,
      programName: String
  ) = {
    this(
      environment,
      node,
      reaction,
      rng,
      programName,
      FastMath.nextUp(1.0 / reaction.getTimeDistribution.getRate),
      ExecuteAlways
    )
  }

  import RunScafiProgram.NBRData
  val program =
    ResourceLoader.classForName(programName).getDeclaredConstructor().newInstance().asInstanceOf[CONTEXT => EXPORT]
  val programNameMolecule = new SimpleMolecule(programName)
  lazy val nodeManager = new SimpleNodeManager(node)
  private var nbrData: Map[ID, NBRData[P]] = Map()
  private var completed = false
  declareDependencyTo(Dependency.EVERY_MOLECULE)

  def asMolecule = programNameMolecule

  override def cloneAction(n: Node[T], r: Reaction[T]) =
    new RunScafiProgram(environment, n, r, rng, programName, retentionTime, scheduling)

  override def execute(): Unit = {
    import scala.jdk.CollectionConverters._
    implicit def euclideanToPoint(p: P): Point3D = p.getDimensions match {
      case 1 => Point3D(p.getCoordinate(0), 0, 0)
      case 2 => Point3D(p.getCoordinate(0), p.getCoordinate(1), 0)
      case 3 => Point3D(p.getCoordinate(0), p.getCoordinate(1), p.getCoordinate(2))
    }
    lazy val position: P = environment.getPosition(node)
    // NB: We assume it.unibo.alchemist.model.interfaces.Time = DoubleTime
    //     and that its "time unit" is seconds, and then we get NANOSECONDS
    val alchemistCurrentTime = Try(environment.getSimulation)
      .map(_.getTime)
      .orElse(
        Failure(new IllegalStateException("The simulation is uninitialized (did you serialize the environment?)"))
      )
      .get
    def alchemistTimeToNanos(time: AlchemistTime): Long = (time.toDouble * 1_000_000_000).toLong
    lazy val currentTime: Long = alchemistTimeToNanos(alchemistCurrentTime)
    if (!nbrData.contains(node.getId)) {
      nbrData += node.getId -> NBRData(factory.emptyExport(), position, Double.NaN)
    }
    nbrData = nbrData.filter { case (id, data) =>
      id == node.getId || data.executionTime >= alchemistCurrentTime - retentionTime
    }
    lazy val deltaTime: Long =
      currentTime - nbrData.get(node.getId).map(d => alchemistTimeToNanos(d.executionTime)).getOrElse(0L)
    lazy val localSensors = node.getContents().asScala.map { case (k, v) => k.getName -> v }

    lazy val nbrSensors = scala.collection.mutable.Map[NSNS, Map[ID, Any]]()
    lazy val exports: Iterable[(ID, EXPORT)] = nbrData.view.mapValues(_.exportData)
    lazy val ctx = new ContextImpl(node.getId, exports, localSensors, Map.empty) {
      override def nbrSense[T](nsns: NSNS)(nbr: ID): Option[T] =
        nbrSensors
          .getOrElseUpdate(
            nsns,
            nsns match {
              case NBR_LAG =>
                nbrData.mapValuesStrict[FiniteDuration](nbr =>
                  FiniteDuration(alchemistTimeToNanos(alchemistCurrentTime - nbr.executionTime), TimeUnit.NANOSECONDS)
                )
              /*
               * nbrDelay is estimated: it should be nbr(deltaTime), here we suppose the round frequency
               * is negligibly different between devices.
               */
              case NBR_DELAY =>
                nbrData.mapValuesStrict[FiniteDuration](nbr =>
                  FiniteDuration(
                    alchemistTimeToNanos(nbr.executionTime) + deltaTime - currentTime,
                    TimeUnit.NANOSECONDS
                  )
                )
              case NBR_RANGE => nbrData.mapValuesStrict[Double](_.position.distanceTo(position))
              case NBR_VECTOR => nbrData.mapValuesStrict[Point3D](_.position.minus(position.getCoordinates))
              case NBR_ALCHEMIST_LAG => nbrData.mapValuesStrict[Double](alchemistCurrentTime - _.executionTime)
              case NBR_ALCHEMIST_DELAY =>
                nbrData.mapValuesStrict(nbr => alchemistTimeToNanos(nbr.executionTime) + deltaTime - currentTime)
            }
          )
          .get(nbr)
          .map(_.asInstanceOf[T])

      override def sense[T](lsns: String): Option[T] = (lsns match {
        case LSNS_ALCHEMIST_COORDINATES => Some(position.getCoordinates)
        case LSNS_DELTA_TIME => Some(FiniteDuration(deltaTime, TimeUnit.NANOSECONDS))
        case LSNS_POSITION =>
          val k = position.getDimensions()
          Some(
            Point3D(
              position.getCoordinate(0),
              if (k >= 2) position.getCoordinate(1) else 0,
              if (k >= 3) position.getCoordinate(2) else 0
            )
          )
        case LSNS_TIMESTAMP => Some(currentTime)
        case LSNS_TIME => Some(java.time.Instant.ofEpochMilli((alchemistCurrentTime * 1000).toLong))
        case LSNS_ALCHEMIST_NODE_MANAGER => Some(nodeManager)
        case LSNS_ALCHEMIST_DELTA_TIME =>
          Some(
            alchemistCurrentTime.minus(nbrData.get(node.getId).map(_.executionTime).getOrElse(AlchemistTime.INFINITY))
          )
        case LSNS_ALCHEMIST_ENVIRONMENT => Some(environment)
        case LSNS_ALCHEMIST_RANDOM => Some(rng)
        case LSNS_ALCHEMIST_TIMESTAMP => Some(alchemistCurrentTime)
        case _ => localSensors.get(lsns)
      }).map(_.asInstanceOf[T])
    }
    scheduling.eval(ctx, new SimpleNodeManager(node)) {
      val computed = program(ctx)
      node.setConcentration(programName, computed.root[T]())
      val toSend = NBRData(computed, position, alchemistCurrentTime)
      nbrData = nbrData + (node.getId -> toSend)
      completed = true
      computed
    }
  }

  def sendExport(id: ID, exportData: NBRData[P]): Unit = nbrData += id -> exportData

  def getExport(id: ID): Option[NBRData[P]] = nbrData.get(id)

  def isComputationalCycleComplete: Boolean = completed

  def prepareForComputationalCycle: Unit = completed = false

}

object RunScafiProgram {
  case class NBRData[P <: Position[P]](exportData: EXPORT, position: P, executionTime: AlchemistTime)

  implicit class RichMap[K, V](m: Map[K, V]) {
    def mapValuesStrict[T](f: V => T): Map[K, T] = m.map(tp => tp._1 -> f(tp._2))
  }
}
