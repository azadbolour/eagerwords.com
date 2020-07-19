/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.domain

/**
  * Types of play.
  */
object PlayType {

  val WordString: String = "Word"
  val SwapString: String = "Swap"

  sealed abstract class PlayType

  object WordPlayType extends PlayType {
    override def toString = WordString
  }

  object SwapPlayType extends PlayType {
    override def toString = SwapString
  }

  def fromString(asString: String): PlayType = {
    asString match {
      case WordString => WordPlayType
      case SwapString => SwapPlayType
      case _ => throw new RuntimeException(s"unrecognized play type: ${asString}")
    }
  }

}

