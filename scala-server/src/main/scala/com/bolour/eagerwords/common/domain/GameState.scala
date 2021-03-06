/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.domain

import com.bolour.util.domain.BaseExceptions.InternalAppException

object GameState {

  sealed abstract class GameState {
    override def toString: String = this match {
      case RUNNING => "RUNNING"
      case RESIGNED => "RESIGNED"
      case ENDED => "ENDED"
    }
  }

  object RUNNING extends GameState

  object RESIGNED extends GameState

  object ENDED extends GameState

  def fromString(name: String): GameState = name match {
    case "RUNNING" => RUNNING
    case "ENDED" => ENDED
    case "RESIGNED" => RESIGNED
    case _ => throw InternalAppException(s"unrecognized game state name: $name", null)
  }

  def isRunning(gameState: GameState): Boolean = gameState == RUNNING
  def isCompleted(gameState: GameState): Boolean = gameState == ENDED || gameState == RESIGNED

}
