/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

object PlayerType {

  val UserString = "User"
  val MachineString = "Machine"

  val RandomPlayerType = "Random"

  sealed abstract class PlayerType

  object UserPlayer extends PlayerType {
    override def toString = UserString
  }

  object MachinePlayer extends PlayerType {
    override def toString = MachineString
  }

  def playerIndex(playerType: PlayerType): Int = playerType match {
    case UserPlayer => 0
    case MachinePlayer => 1
  }

  def nextPlayerType(playerType: PlayerType): PlayerType =
    playerType match {
      case UserPlayer => MachinePlayer
      case MachinePlayer => UserPlayer
    }

  def fromString(asString: String): PlayerType = {
    asString match {
      case UserString => PlayerType.UserPlayer
      case MachineString => PlayerType.MachinePlayer
      case _ => throw new RuntimeException(s"unrecognized player type: ${asString}")
    }
  }
}

