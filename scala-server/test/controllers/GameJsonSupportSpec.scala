/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package controllers

import com.bolour.eagerwords.common.domain.DeviceType.MouseDevice
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsError, JsString, JsSuccess, Json}
import com.bolour.eagerwords.common.domain.PieceProviderType.PieceProviderType
import com.bolour.eagerwords.common.domain.PlayType.{PlayType, WordPlayType}
import com.bolour.eagerwords.common.domain.Play._
import com.bolour.eagerwords.common.domain.PlayerType.UserPlayer
import com.bolour.eagerwords.common.domain.{GameParams, GameSettings, GameState, Piece, PiecePoint, PieceProviderType, Play, PlayPiece, PlayerType, SwapPlay, WordPlay}
import com.bolour.eagerwords.common.message.GetFullGameResponse
import com.bolour.plane.domain.Point
import controllers.GameApiJsonSupport._

class GameJsonSupportSpec extends FlatSpec with Matchers {

  val logger = LoggerFactory.getLogger(this.getClass)

  "piece provider type info" should "get values" in {
    val jsonString = "Cyclic"
    val jsValue = JsString(jsonString)
    val result = Json.fromJson[PieceProviderType](jsValue)
    result match {
      case JsSuccess(genType, _) => println(s"${genType}")
      case JsError(ex) =>
        logger.error(s"${ex}")
        throw new Exception(s"${ex}")
    }
    println(s"${result}")
  }

  "play type" should "be written and read correctly to/from json" in {
    val wordPlayTypeJsValue = Json.toJson(WordPlayType)
    logger.info(s"word play type json: ${wordPlayTypeJsValue.toString()}")

    val result = Json.fromJson[PlayType](wordPlayTypeJsValue)
    result match {
      case JsSuccess(playType, _) => println(s"${playType}")
      case JsError(ex) =>
        logger.error(s"${ex}")
        throw new Exception(s"${ex}")
    }
  }

  val piece: Piece = Piece('A', "idA")
  val point: Point = Point(1, 1)
  val playPiece: PlayPiece = PlayPiece(piece, point, true);
  val repPiece: Piece = Piece('B', "idB")
  val wordPlay: Play = mkWordPlay(1, UserPlayer, List(10, 20), List(playPiece), List(repPiece), Nil)

  // TODO. URGENT. Add json test for swap play.
  "word play" should "be written and read correctly to/from json" in {
    Json.toJson(wordPlay)
    val wordPlayJsValue = Json.toJson(wordPlay)
    logger.info(s"word play json: ${wordPlayJsValue.toString()}")

    val result = Json.fromJson[Play](wordPlayJsValue)
    result match {
      case JsSuccess(play, _) =>
        println(s"${play}")
        play match {
          case wordPlay: WordPlay =>
            wordPlay.playerType should equal (UserPlayer)
            wordPlay.playPieces(0).piece should equal (piece)
          case _: SwapPlay =>
            throw new Exception("word play decoded to swap play")
        }
      case JsError(ex) =>
        logger.error(s"${ex}")
        throw new Exception(s"${ex}")
    }
  }

  val dimension = 5
  val squarePixels = 20
  val trayCapacity = 5
  val languageCode = "tiny"
  val startingPlayer = PlayerType.UserPlayer

  val settings = GameSettings(dimension, squarePixels, trayCapacity, languageCode,
    PieceProviderType.Cyclic, Some(startingPlayer), Some(MouseDevice))
  val pointValues = List.fill(dimension, dimension)(1)
  val gameParams = GameParams(settings, pointValues)

  "get full game response" should "be written and read to/from json" in {
    val getFullGameResponse = GetFullGameResponse(
      "12345678",
      gameParams,
      List(PiecePoint(piece, point)),
      List(Piece('C', "idC")),
      10,
      20,
      2,
      UserPlayer,
      Vector(wordPlay),
      GameState.RUNNING,
      List()
    )

    val jsValue = Json.toJson(getFullGameResponse)
    val result = Json.fromJson[GetFullGameResponse](jsValue)

    result match {
      case JsSuccess(response, _) =>
        response should equal (getFullGameResponse)
      case JsError(errors) =>
        logger.error(s"${errors}")
        throw new Exception(s"${errors}")
    }
  }
}
