/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

/**
  * Properties of a game to be started.
  *
  * @param playParams Settable parameters for starting games.
  * @param pointValues Values attached to each board square for scoring.
  */
case class GameParams(
  playParams: GamePlayParams,
  pointValues: List[List[Int]]
)

