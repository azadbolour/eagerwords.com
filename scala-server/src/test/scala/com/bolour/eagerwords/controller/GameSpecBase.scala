package com.bolour.eagerwords.controller

import com.bolour.util.CommonUtil.Email
import com.bolour.eagerwords.common.domain.{GameParams, GamePlayParams, PieceProviderType, PlayerType}

trait GameSpecBase {
  val name = "Bill"
  val email: Email = Email("bill@example.com")
  val userId = "12345"

  val dimension = 5
  val squarePixels = 20
  val trayCapacity = 5
  val languageCode = "tiny"
  val startingPlayer = PlayerType.UserPlayer
  val genType = PieceProviderType.Cyclic

  val pointValues = List.fill(dimension, dimension)(1)

  val playSettings: GamePlayParams = GamePlayParams(dimension, trayCapacity, languageCode, genType, Some(startingPlayer))
  val gameParams = GameParams(playSettings, pointValues)

  // val gameParams = GameParams(settings, pointValues)
  val center = dimension/2

}
