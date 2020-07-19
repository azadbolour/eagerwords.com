/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.message

import com.bolour.eagerwords.common.domain.GameState.GameState
import com.bolour.eagerwords.common.domain.PlayerType.PlayerType
import com.bolour.eagerwords.common.domain.{GameParams, Piece, PiecePoint, Play}

case class GetFullGameResponse(
  gameId: String,
  gameParams: GameParams,
  boardPiecePoints: List[PiecePoint],
  trayPieces: List[Piece],
  userScore: Int,
  machineScore: Int,
  playNumber: Int,
  playTurn: PlayerType,
  plays: Vector[Play],
  state: GameState
)
