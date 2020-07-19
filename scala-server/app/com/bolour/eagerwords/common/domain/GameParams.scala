/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

import com.bolour.eagerwords.common.domain.DeviceType.DeviceType
import com.bolour.eagerwords.common.domain.PieceProviderType.PieceProviderType
import com.bolour.eagerwords.common.domain.PlayerType.PlayerType

/**
  * Properties of a game to be started.
  *
  * @param settings Game parameters that can be set by users.
  * @param pointValues Values attached to each board square for scoring.
  */
case class GameParams(
  settings: GameSettings,
  pointValues: List[List[Int]]
) {
  def dimension: Int = settings.dimension
  // TODO. Use lenses.
  def dimension(d: Int): GameParams =
    this.copy(settings = this.settings.copy(dimension = d))

  def squarePixels: Int = settings.squarePixels
  def squarePixels(sq: Int): GameParams =
    this.copy(settings = this.settings.copy(squarePixels = sq))

  def trayCapacity: Int = settings.trayCapacity
  def trayCapacity(c: Int): GameParams =
    this.copy(settings = this.settings.copy(trayCapacity = c))

  def languageCode: String = settings.languageCode
  def languageCode(c: String): GameParams =
    this.copy(settings = this.settings.copy(languageCode = c))

  def pieceProviderType: PieceProviderType = settings.pieceProviderType
  def pieceProvideType(t: PieceProviderType): GameParams =
    this.copy(settings = this.settings.copy(pieceProviderType = t))

  def startingPlayer: Option[PlayerType] = settings.startingPlayer
  def staringPlayer(p: Option[PlayerType]): GameParams =
    this.copy(settings = this.settings.copy(startingPlayer = p))

  def preferredDevice: Option[DeviceType] = settings.preferredDevice
  def preferredDevice(d: Option[DeviceType]): GameParams =
    this.copy(settings = this.settings.copy(preferredDevice = d))
}
