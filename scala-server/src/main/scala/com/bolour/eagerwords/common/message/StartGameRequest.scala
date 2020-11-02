/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.message

import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.eagerwords.common.domain.{GameParams, InitPieces}

/**
  * API request to start a game.
  *
  * @param loginEvidence Client id and token evidence of of the logged-in user. None for guest.
  * @param gameParams Basic specification of the game.
  * @param initPieces Initial pieces on the board - for testing.
  */
case class StartGameRequest(
  loginEvidence: Option[AuthEvidence],
  gameParams: GameParams,
  initPieces: InitPieces,
)
