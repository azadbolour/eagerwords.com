/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

object PlayerType {

  val UserString = "User"
  val MachineString = "Machine"

  val RandomString = "Random"

  sealed abstract class PlayerType

  object UserPlayer extends PlayerType {
    override def toString = UserString
  }

  object MachinePlayer extends PlayerType {
    override def toString = MachineString
  }

  object RandomPlayer extends PlayerType {
    override def toString = RandomString
  }

  def playerIndex(playerType: PlayerType): Int = playerType match {
    case UserPlayer => 0
    case MachinePlayer => 1
    case RandomPlayer => throw new IllegalArgumentException(s"cannot index random player")
  }

  def nextPlayerType(playerType: PlayerType): PlayerType =
    playerType match {
      case UserPlayer => MachinePlayer
      case MachinePlayer => UserPlayer
      case RandomPlayer => throw new IllegalArgumentException(s"next player is meaningless for random player")
    }

  def fromString(asString: String): PlayerType = {
    asString match {
      case UserString => PlayerType.UserPlayer
      case MachineString => PlayerType.MachinePlayer
      case RandomString => PlayerType.RandomPlayer
      case _ => throw new RuntimeException(s"unrecognized player type: ${asString}")
    }
  }
}

