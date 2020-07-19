package com.bolour.eagerwords.server.domain.json

import java.time.Instant

import com.bolour.eagerwords.common.domain.DeviceType.MouseDevice
import com.bolour.eagerwords.common.domain.PlayerType.PlayerType
import com.bolour.eagerwords.common.domain.{GameParams, GameSettings, InitPieces, Piece, PieceProviderType, PlayerType}
import com.bolour.eagerwords.common.domain.GameState.GameState
import com.bolour.eagerwords.server.domain.{Board, CyclicPieceProvider, Game, GameBase, GameObj, PieceProvider, RandomPieceProvider, Tray}
import com.bolour.util.FrequencyDistribution
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

class CirceSpec extends FlatSpec with Matchers {
  val logger = LoggerFactory.getLogger(this.getClass)

  // implicit val fooDecoder: Decoder[Game] = deriveDecoder[Game]
  // implicit val fooEncoder: Encoder[Game] = deriveEncoder[Game]

  val valueList = "ABCD"
  val cyclicPieceProvider: PieceProvider = CyclicPieceProvider(valueList, 0)
  val startingPlayer = PlayerType.UserPlayer
  val squarePixels = 20

  val dimension = 9
  val trayCapacity = 3
  val tinyLang = "tiny"
  val pointValues = List.fill(dimension, dimension)(1)
  val initPieces = InitPieces(List(), List.fill(trayCapacity)(Piece('A', "A")), List.fill(trayCapacity)(Piece('A', "A")))

  val settings = GameSettings(dimension, squarePixels, trayCapacity, tinyLang,
    PieceProviderType.Random, Some(startingPlayer), Some(MouseDevice))

  val gameParams = GameParams(settings, pointValues)
  val userId = "12345678"
  val gameBase = GameBase(gameParams, initPieces, Some(userId))
  val game = GameObj.mkGame(gameBase).get

  val board = Board(dimension)

  "cyclic piece provider" should "encode and decode to/from json" in {
    val circeJson: Json = cyclicPieceProvider.asJson
    val jsonString: String = circeJson.toString
    // logger.info(jsonString)
    val parseResult = decode[PieceProvider](jsonString)
    parseResult match {
      case Right(pieceProvider) =>
        // logger.info(s"$pieceProvider")
        pieceProvider shouldEqual cyclicPieceProvider
      case Left(error) =>
        throw error
    }
  }

  "random piece provider" should "encode and decode to/from json" in {
    val frequencies = List(('A', 10), ('B', 20))
    val frequencyDistribution = FrequencyDistribution(frequencies)
    val randomPieceProvider: PieceProvider = RandomPieceProvider(frequencyDistribution, 0)

    val circeJson: Json = randomPieceProvider.asJson
    val jsonString: String = circeJson.toString
    // logger.info(jsonString)
    val parseResult = decode[PieceProvider](jsonString)
    parseResult match {
      case Right(pieceProvider) =>
        // logger.info(s"$pieceProvider")
        pieceProvider shouldEqual randomPieceProvider
      case Left(error) =>
        throw error
    }
  }

  // TODO. Generic encode and decode for any case class - possible?
  "board" should "encode and decode to/from json" in {
    // val board = Board(dimension)
    val circeJson: Json = board.asJson
    val jsonString: String = circeJson.toString
    // logger.info(jsonString)
    val parseResult = decode[Board](jsonString)
    parseResult match {
      case Right(decoded) =>
        // logger.info(s"$decoded")
        decoded shouldEqual board
      case Left(error) =>
        throw error
    }
  }

  "game" should "encode and decode to/from json" in {
    val circeJson: Json = game.asJson
    val jsonString: String = circeJson.toString
    // logger.info(jsonString)
    val parseResult = decode[Game](jsonString)
    parseResult match {
      case Right(decoded) =>
        // logger.info(s"$decoded")
        decoded shouldEqual game
      case Left(error) =>
        throw error
    }
  }
}
