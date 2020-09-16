/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.domain

import com.bolour.eagerwords.common.domain.DeviceType.DeviceType
import com.bolour.eagerwords.common.domain.PieceProviderType.PieceProviderType
import com.bolour.eagerwords.common.domain.PlayerType.PlayerType

/**
  * User's game settings.
  *
  * @param playSettings: Settable preference parameters for starting games.
  * @param lookAndFeelSettings: Settable parameters affecting the look and feel of games.
  */
case class UserGameSettings(
  playSettings: GamePlayParams,
  lookAndFeelSettings: GameLookAndFeelParams
)
