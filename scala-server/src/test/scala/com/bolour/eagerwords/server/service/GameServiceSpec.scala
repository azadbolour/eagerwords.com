/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.server.service

import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.ScalaFutures.{PatienceConfig, _}
import org.scalatest.time.{Millis, Seconds, Span}
import org.slf4j.LoggerFactory

import com.bolour.util.MiscUtil.stringId
import com.bolour.util.TimeUtil.nowSecs
import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.auth.server.service.{AuthServiceImpl, SecretServiceConfigImpl}
import com.bolour.grid.domain.Point
import com.bolour.eagerwords.common.domain.{GameState, _}
import com.bolour.eagerwords.common.domain.DeviceType.MouseDevice
import com.bolour.eagerwords.server.domain.Game
import com.bolour.eagerwords.server.service.ServiceTestHelper.testingSignUp

class GameServiceSpec extends FlatSpec with Matchers {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val timeout = 5.seconds

  // Needed for futureValue. Hides the default provided by scalatest.
  implicit val patienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

  val dimension = 5
  val trayCapacity = 2
  val startingPlayer = PlayerType.UserPlayer
  val squarePixels = 20

  val center = dimension / 2
  val tinyLang = "tiny"

  val myUserId = UUID.randomUUID().toString;
  val email = s"${myUserId}@example.com"
  val nickname = myUserId

  // val userId = "12345678"

  val genType = PieceProviderType.Random // TODO. Change to cyclic.

  val pointValues = List.fill(dimension, dimension)(1)
  val playSettings: GamePlayParams = GamePlayParams(dimension, trayCapacity, tinyLang, genType, Some(startingPlayer))
  val gameParams = GameParams(playSettings, pointValues)

//  val settings = GameSettings(dimension, squarePixels, trayCapacity, tinyLang, genType,
//    Some(startingPlayer), Some(MouseDevice))
//
//  val gameParams = GameParams(settings, pointValues)

  val conf = ConfigFactory.load()
  val secretService = new SecretServiceConfigImpl(conf)
  val appService = new AuthServiceImpl(conf, secretService, None)
  val service = new GameServiceImpl(ConfigFactory.load(), None, appService)
  appService.migrate().futureValue
  service.migrate().futureValue

  // appService.saveUser(userId, name, email, false).futureValue

  val signUpEvidence: AuthEvidence = testingSignUp(appService)(email, nickname)

  def piecePoint(letter: Char, row: Int, col: Int) = PiecePoint(Piece(letter, stringId()), Point(row, col))

  val top = piecePoint('S', center - 1, center)
  val bottom = piecePoint('T', center + 1, center)
  // _ S _
  // B E T
  // _ T _
  val piecePoints = List(
    piecePoint('B', center, center - 1),
    piecePoint('E', center, center),
    piecePoint('T', center, center + 1),
    top,
    bottom,
  )

  def startGameAndCommitPlay(
    initUserPieces: List[Piece],
    playPieces: List[PlayPiece],
    maybeEmail: Option[String] = Some(email)
  ) = {

    val initPieces = InitPieces(piecePoints, initUserPieces, List())
    for {
      game <- service.startGame(gameParams, initPieces, maybeEmail)
      (miniState, replacementPieces, deadPoints) <- service.commitPlay(game.gameBase.gameId, playPieces)
    } yield (game, miniState, replacementPieces)
  }

  def startGameAndCommitDefaultPlay(maybeEmail: Option[String] = Some(email)): Game = {
    val uPieces = List(Piece('O', stringId()), Piece('O', stringId()))
    val playPieces = List(
      PlayPiece(bottom.piece, bottom.point, false),
      // Add O to the bottom right getting word TO and crossword TO (which is valid).
      PlayPiece(uPieces(0), Point(center + 1, center + 1), true)
    )

    val (originalGame, _, _) = startGameAndCommitPlay(uPieces, playPieces, maybeEmail).futureValue
    val gameId = originalGame.gameBase.gameId
    val committedGame: Game = service.findGameById(gameId).futureValue.get
    committedGame
  }

  "game service" should "accept valid crosswords" in {
    // Allow only O to be used.
    val uPieces = List(Piece('O', stringId()), Piece('O', stringId()))
    val playPieces = List(
      PlayPiece(bottom.piece, bottom.point, false),
      // Add O to the bottom right getting word TO and crossword TO (which is valid).
      PlayPiece(uPieces(0), Point(center + 1, center + 1), true)
    )
    val (_, _, replacementPieces) = startGameAndCommitPlay(uPieces, playPieces, None).futureValue
    replacementPieces.length shouldBe (1)
  }

  "game service" should "reject invalid crosswords" in {
    // Allow only O to be used.
    val uPieces = List(Piece('O', stringId()), Piece('O', stringId()))
    val playPieces = List(
      PlayPiece(top.piece, top.point, false),
      // Add O to the top right getting word SO and crossword OT (which is invalid).
      PlayPiece(uPieces(0), Point(center - 1, center + 1), true)
    )
    val future = startGameAndCommitPlay(uPieces, playPieces)
    Await.ready(future, timeout)
    future.value.get match {
      case Success(_) => throw new Exception("")
      case Failure(_) =>
    }
    // Not sure why this suddenly started got timeouts.
    //    whenReady(startGameAndCommitPlay(uPieces, playPieces).failed) { e =>
    //      e shouldBe an [Exception]
    //    }

  }

  "game service" should "store and retrieve game" in {
    val uPieces = List(Piece('O', stringId()), Piece('O', stringId()))
    val playPieces = List(
      PlayPiece(bottom.piece, bottom.point, false),
      // Add O to the bottom right getting word TO and crossword TO (which is valid).
      PlayPiece(uPieces(0), Point(center + 1, center + 1), true)
    )
    val future = for {
      (startedGame, miniState, _) <- startGameAndCommitPlay(uPieces, playPieces)
      retrieved <- service.findGameById(startedGame.gameBase.gameId)
    } yield (startedGame, miniState, retrieved)

    val (startedGame, miniState, retrieved) = future.futureValue
    val foundGame = retrieved.get
    foundGame.gameBase shouldEqual startedGame.gameBase
    miniState.scores shouldEqual List(1, 0)
  }

  "game service" should "find unfinished games" in {
    val uPieces = List(Piece('O', stringId()), Piece('O', stringId()))
    val playPieces = List(
      PlayPiece(bottom.piece, bottom.point, false),
      // Add O to the bottom right getting word TO and crossword TO (which is valid).
      PlayPiece(uPieces(0), Point(center + 1, center + 1), true)
    )
    val future = for {
      _ <- service.reset()
      (originalGame, _, _) <- startGameAndCommitPlay(uPieces, playPieces)
      unfinishedGames <- service.getUnfinishedUserGames(email)
    } yield (originalGame, unfinishedGames)
    val (originalGame, unfinishedGames) = future.futureValue
    val unfinishedBase = unfinishedGames.map(_.gameBase).head
    unfinishedBase shouldEqual (originalGame.gameBase)
  }

  "game service" should "find games" in {
    val maxGames = 1;
    val uPieces = List(Piece('O', stringId()), Piece('O', stringId()))
    val playPieces = List(
      PlayPiece(bottom.piece, bottom.point, false),
      // Add O to the bottom right getting word TO and crossword TO (which is valid).
      PlayPiece(uPieces(0), Point(center + 1, center + 1), true)
    )
    val future = for {
      _ <- service.reset()
      (originalGame, _, _) <- startGameAndCommitPlay(uPieces, playPieces)
      // TODO. Add time restrictions to getting all games. For now they are ignored.
      games <- service.getUserGames(email, 0, 0, maxGames)
    } yield (originalGame, games)
    val (originalGame, games) = future.futureValue
    val base = games.map(_.gameBase).head
    base shouldEqual (originalGame.gameBase)
  }

//  "game service" should "suspend and resume games" in {
//    val uPieces = List(Piece('O', stringId()), Piece('O', stringId()))
//    val playPieces = List(
//      PlayPiece(bottom.piece, bottom.point, false),
//      // Add O to the bottom right getting word TO and crossword TO (which is valid).
//      PlayPiece(uPieces(0), Point(center + 1, center + 1), true)
//    )
//
//    val (originalGame, _, _) = startGameAndCommitPlay(uPieces, playPieces).futureValue
//    val gameId = originalGame.gameBase.gameId
//
//    val suspended = service.findGameById(gameId).futureValue.get
//
//    service.suspendGame(gameId).futureValue
//    val resumed = service.resumeGame(gameId).futureValue
//    resumed.state shouldEqual GameState.RUNNING
//    resumed.board shouldEqual suspended.board
//  }

  "game service" should "cancel (forget) game" in {
    val game = startGameAndCommitDefaultPlay()
    val gameId = game.gameBase.gameId
    service.cancelGame(gameId).futureValue
    val maybeGame: Option[Game] = service.findGameById(gameId).futureValue
    maybeGame.isEmpty shouldEqual true
  }

  // Nice to have. Add a machine play and a swap then resign in the resign tests.

  "game service" should "resign game for registered user" in {
    val game = startGameAndCommitDefaultPlay()
    val gameId = game.gameBase.gameId
    service.resignGame(gameId).futureValue
    val resignedGame: Game = service.findGameById(gameId).futureValue.get
    resignedGame.state shouldEqual GameState.RESIGNED
    val lastSecond = resignedGame.lastSecond
    val initialGameAsResigned = game.copy(state = GameState.RESIGNED, lastSecond = lastSecond)
    resignedGame shouldEqual initialGameAsResigned
  }

  "game service" should "resign (forget) game for guest user" in {
    val game = startGameAndCommitDefaultPlay(None)
    val gameId = game.gameBase.gameId
    service.resignGame(gameId).futureValue
    val maybeGame: Option[Game] = service.findGameById(gameId).futureValue
    maybeGame.isEmpty shouldEqual true
  }

  "game service" should "remove all user game related information" in {
    // Start a game for the user.
    val game = startGameAndCommitDefaultPlay()
    val gameId = game.gameBase.gameId
    val someGame = service.findGameById(gameId).futureValue
    someGame.isDefined shouldEqual true
    someGame.get.gameBase.gameId shouldEqual gameId

    // Save user settings.
    val lookAndFeelSettings = GameLookAndFeelParams(SquareSize.NormalSquare, Some(MouseDevice))
    val settings = UserGameSettings(playSettings, lookAndFeelSettings)
    service.saveUserGameSettings(email, settings).futureValue
    val maybeSettings = service.getUserGameSettings(email).futureValue
    maybeSettings.isDefined shouldEqual true
    maybeSettings.get shouldEqual settings

    service.removeAllUserGameRelatedInfo(email).futureValue

    val games = service.getUserGames(email, 0L, nowSecs, 10).futureValue
    games.size shouldBe 0

    val noSettings = service.getUserGameSettings(email).futureValue
    noSettings.isEmpty shouldBe true

    val noGame = service.findGameById(gameId).futureValue
    noGame.isEmpty shouldEqual true
  }

  "game service" should "purge long-running games" in {
    val game = startGameAndCommitDefaultPlay(None)
    val gameId = game.gameBase.gameId
    val someGame = service.findGameById(gameId).futureValue
    someGame.isDefined shouldEqual true
    service.timeoutLongRunningGames(0L).futureValue
    val noGame = service.findGameById(gameId).futureValue
    noGame.isEmpty shouldEqual true
  }

}
