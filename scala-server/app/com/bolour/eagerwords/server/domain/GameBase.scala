/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.domain

import java.time.Instant

import com.bolour.eagerwords.common.domain.PieceProviderType.PieceProviderType
import com.bolour.util.CommonUtil.ID
import com.bolour.app.util.server.KernelUtil.stringId
import com.bolour.eagerwords.server.util.WordUtil.english
import com.bolour.eagerwords.common.domain.{GameParams, InitPieces}

// @param pointValues Values attached to each board square for scoring.

/**
  * Properties of a game that are conceptually immutable once set.
  *
  * @param initPieces Initial pieces on board and in trays.
  * @param playerId Unique internal id of the user player. None for guests.
  * @param firstSecond Time game began.
  */
case class GameBase(
  gameId: ID,
  gameParams: GameParams,
  initPieces: InitPieces,
  playerId: Option[ID],
  firstSecond: Long
) {
  def scorer = Scorer(gameParams.dimension, gameParams.trayCapacity, gameParams.pointValues)
}

object GameBase {
  def apply(
    gameParams: GameParams,
    initPieces: InitPieces,
    playerId: Option[ID],
  ): GameBase = {
    val seconds = Instant.now().getEpochSecond;
    val languageCode = gameParams.languageCode
    val realLanguageCode = if (!languageCode.isEmpty) languageCode else english
    val params = gameParams.languageCode(realLanguageCode)
    GameBase(stringId(), params, initPieces, playerId, seconds)
  }

}

