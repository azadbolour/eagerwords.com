/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.message

import com.bolour.eagerwords.common.domain.{GameParams, Piece, PiecePoint}

/**
  * API response for starting a game.
  *
  * @param gameId Unique id of the game.
  * @param boardPiecePoints The initial state of the board. TODO. Unnecessary.
  * @param trayPieces The user's initial tray  pieces.
  */
case class StartGameResponse(
  gameId: String,
  boardPiecePoints: List[PiecePoint],
  trayPieces: List[Piece]
)
