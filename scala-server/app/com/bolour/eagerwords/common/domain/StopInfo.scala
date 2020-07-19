/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

/**
  * Information about a completed game.
  *
  * TODO. Use sealed abstract case class for different reasons the game stopped.
  *
  * @param successivePasses Number of successive passes (was maxed out).
  * @param filledBoard The board has been filled and no more play is possible.
  */
case class StopInfo(
  successivePasses: Int,
  filledBoard: Boolean
)
