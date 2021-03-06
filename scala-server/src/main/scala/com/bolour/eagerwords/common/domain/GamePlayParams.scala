/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.domain

import com.bolour.eagerwords.common.domain.PieceProviderType.PieceProviderType
import com.bolour.eagerwords.common.domain.PlayerType.PlayerType

/**
  * User's game settings.
  *
  * @param dimension The dimension of the board.
  * @param trayCapacity The number of pieces (tiles) in each player's tray.
  * @param languageCode The id of the game's language. Determines the word dictionary to use.
  *                     This is the standard underscore-separated locale identifier.
  * @param pieceProviderType The type of factory to use for generating new pieces (tiles).
  * @param startingPlayer User player of machine player to start the game (Some).
  *                       None means use a random player.
  */
case class GamePlayParams(
  dimension: Int,
  trayCapacity: Int,
  languageCode: String,
  pieceProviderType: PieceProviderType,
  startingPlayer: Option[PlayerType]
)
