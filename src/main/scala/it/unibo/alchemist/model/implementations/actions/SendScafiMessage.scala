/*
 * Copyright (C) 2010-2019, Danilo Pianini and contributors
 * listed in the main project's alchemist/build.gradle.kts file.
 *
 * This file is part of Alchemist, and is distributed under the terms of the
 * GNU General Public License, with a linking exception,
 * as described in the file LICENSE in the Alchemist distribution's top directory.
 */

package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.ScafiIncarnationUtils
import it.unibo.alchemist.model.ScafiIncarnationUtils._
import it.unibo.alchemist.model.implementations.nodes.ScafiNode
import it.unibo.alchemist.model.interfaces._
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

class SendScafiMessage[T, P <: Position[P]](
    env: Environment[T, P],
    node: ScafiNode[T, P],
    reaction: Reaction[T],
    val program: RunScafiProgram[T, P]
) extends AbstractAction[T](node) {
  assert(reaction != null, "Reaction cannot be null")
  assert(program != null, "Program cannot be null")

  /** This method allows to clone this action on a new node. It may result useful to support runtime creation of nodes
    * with the same reaction programming, e.g. for morphogenesis.
    *
    * @param n
    *   The node where to clone this { @link Action}
    * @param r
    *   The reaction to which the CURRENT action is assigned
    * @return
    *   the cloned action
    */
  override def cloneAction(n: Node[T], r: Reaction[T]): Action[T] = {
    if (!isScafiNode(n)) {
      throw new IllegalStateException(
        getClass.getSimpleName + " cannot get cloned on a node of type " + node.getClass.getSimpleName
      )
    }
    val destinationNode = n.asInstanceOf[ScafiNode[T, P]]
    val possibleRef = destinationNode.getReactions
      .stream()
      .flatMap(reaction => reaction.getActions.stream())
      .filter(action => action.isInstanceOf[RunScafiProgram[_, _]])
      .map(action => action.asInstanceOf[RunScafiProgram[T, P]])
      .collect(Collectors.toList[RunScafiProgram[T, P]])
    if (possibleRef.size() == 1) {
      return new SendScafiMessage(env, destinationNode, reaction, possibleRef.get(0))
    }
    throw new IllegalStateException(
      "There must be one and one only unconfigured " + RunScafiProgram.getClass.getSimpleName
    )
  }

  /** Effectively executes this action. */
  override def execute(): Unit = {
    val toSend = program.getExport(node.getId).get
    for {
      nbr <- env.getNeighborhood(node).getNeighbors.iterator().asScala
      action <- ScafiIncarnationUtils.allScafiProgramsFor[T, P](nbr).filter(program.getClass.isInstance(_))
    } action.sendExport(node.getId, toSend)
    program.prepareForComputationalCycle
  }

  /** @return The context for this action. */
  override def getContext: Context = Context.NEIGHBORHOOD
}
