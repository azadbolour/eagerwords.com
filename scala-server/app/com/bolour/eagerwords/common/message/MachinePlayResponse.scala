/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.message

import com.bolour.eagerwords.common.domain.{GameMiniState, PlayPiece}
import com.bolour.plane.domain.Point

/**
  * API response to a request to initiate a machine play.
  *
  * @param gameMiniState Mini-state of the game after the play.
  * @param playedPieces List of pieces and locations used in the machine play.
  * @param deadPoints Empty points on the board that have been determined to be
  *                   no longer playable after the machine's play was laid out on the board.
  */
case class MachinePlayResponse(
  gameMiniState: GameMiniState,
  playedPieces: List[PlayPiece],
  deadPoints: List[Point]
)
