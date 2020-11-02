/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.message

import com.bolour.eagerwords.common.domain.{GameParams, Piece, PiecePoint}

/**
  * API response for resuming a game. Deprecated. Resume returns unit.
  * Use getFullGame to get the resumed game.
  *
  * @param gameId Unique id of the game.
  * @param gameParams The game parameters.
  * @param boardPiecePoints The suspended state of the board.
  * @param trayPieces The user's tray pieces at the time of suspension.
  * @param userScore The user's score.
  * @param machineScore The machine's score.
  */
@Deprecated
case class ResumeGameResponse(
  gameId: String,
  gameParams: GameParams,
  boardPiecePoints: List[PiecePoint],
  trayPieces: List[Piece],
  userScore: Int,
  machineScore: Int
)
