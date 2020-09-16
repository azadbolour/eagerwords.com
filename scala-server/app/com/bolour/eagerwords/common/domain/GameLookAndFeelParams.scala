/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.domain

import com.bolour.eagerwords.common.domain.DeviceType.DeviceType
import com.bolour.eagerwords.common.domain.PieceProviderType.PieceProviderType
import com.bolour.eagerwords.common.domain.PlayerType.PlayerType
import com.bolour.eagerwords.common.domain.SquareSize.SquareSize

/**
  * Parameters affecting the look and feel of the game in the UI.
  *
  * @param squareSize The relative size of each square.
  * @param preferredDevice Type of pointing device (optional).
  */
case class GameLookAndFeelParams(
  squareSize: SquareSize,
  preferredDevice: Option[DeviceType]
)

// Note. PreferredDevice is actually a general user preference - not specific to games.
// It properly belongs to the kernel module. Expedient to keep it here for now for simplicity.
