/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.server.domain

import com.bolour.app.util.server.KernelUtil.stringId
import com.bolour.eagerwords.common.domain.DeviceType.MouseDevice
import com.bolour.plane.domain.Axis.Axis
import com.bolour.eagerwords.common.domain._
import com.bolour.eagerwords.common.domain.PlayerType._
import com.bolour.plane.domain.{Axis, Point}
import org.scalatest.{FlatSpec, Matchers}
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success}

// TODO. This test is flaky - depends on random piece generation.

class GameStateSpec extends FlatSpec with Matchers {

  val logger = LoggerFactory.getLogger(this.getClass)
  val genType = PieceProviderType.Cyclic
  val name = "John"

  val caps = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val pieceProvider: PieceProvider = CyclicPieceProvider(caps, 0)
  val startingPlayer = PlayerType.UserPlayer
  val squarePixels = 20

  def mkPlayPieces(startingPoint: Point, axis: Axis, length: Int, pieces: Vector[Piece]): List[PlayPiece] = {
    val row = startingPoint.row
    val col = startingPoint.col
    val vector = pieces.take(length) zip (0 until length) map {
      case (piece, i) =>
        val playPoint = axis match {
          case Axis.X => Point(row, col + i)
          case Axis.Y => Point(row + 1, col)
        }
        PlayPiece(piece, playPoint, true)
    }
    vector.toList
  }

  "initial game state" should "not have duplicate piece ids" in {
    val dimension = 15
    val trayCapacity = 15
    val pointValues = List.fill(dimension, dimension)(1)
    val settings = GameSettings(
      dimension, squarePixels, trayCapacity, "en", genType,
      Some(startingPlayer), Some(MouseDevice))
    val gameParams = GameParams(settings, pointValues)

    val initPieces = InitPieces(List(), List(), List())
    val gameBase = GameBase(gameParams, initPieces, None)
    val result = for {
      game <- GameObj.mkGame(gameBase)
      _ <- game.sanityCheck
    } yield ()

    result match {
      case Success(_) => ()
      case Failure(ex) => fail(ex)
    }
  }

  "play scores" should "be computed correctly" in {
    val dimension = 15
    val trayCapacity = 7

    val pointValues = List.fill(dimension, dimension)(1)
    val settings = GameSettings(dimension, squarePixels, trayCapacity, "en", genType,
      Some(startingPlayer), Some(MouseDevice))

    val gameParams = GameParams(settings, pointValues)
    val initPieces = InitPieces(List(), List(), List())
    val gameBase = GameBase(gameParams, initPieces, Some("123"))

    def pc(ch: Char): Piece = Piece(ch, stringId())

    // Plays wil yield:
    /*
                    2         5
        - - - - - - - - - - - - - - -
      6 - - - - - - - - E G I P T O -
      4 - - - - - C H U T N E E - - -
        - - - - - - R - - - - R - - -
      3 - - - - F A D E - - - U - - -
        - - - - - - W - - - - - - - -
        - - - - - - R - - - - - - - -
        - - - - - V E N D - - - - - -
     */

    val machinePlay1 = List(
      PlayPiece(pc('V'), Point(7, 5), true),
      PlayPiece(pc('E'), Point(7, 6), true),
      PlayPiece(pc('N'), Point(7, 7), true),
      PlayPiece(pc('D'), Point(7, 8), true),
    )

    val userPlay1 = List(
      PlayPiece(pc('H'), Point(2, 6), true),
      PlayPiece(pc('R'), Point(3, 6), true),
      PlayPiece(pc('D'), Point(4, 6), true),
      PlayPiece(pc('W'), Point(5, 6), true),
      PlayPiece(pc('R'), Point(6, 6), true),
      PlayPiece(pc('E'), Point(7, 6), false),
    )

    val machinePlay2 = List(
      PlayPiece(pc('F'), Point(4, 4), true),
      PlayPiece(pc('A'), Point(4, 5), true),
      PlayPiece(pc('D'), Point(4, 6), false),
      PlayPiece(pc('E'), Point(4, 7), true),
    )

    val userPlay2 = List(
      PlayPiece(pc('C'), Point(2, 5), true),
      PlayPiece(pc('H'), Point(2, 6), false),
      PlayPiece(pc('U'), Point(2, 7), true),
      PlayPiece(pc('T'), Point(2, 8), true),
      PlayPiece(pc('N'), Point(2, 9), true),
      PlayPiece(pc('E'), Point(2, 10), true),
      PlayPiece(pc('E'), Point(2, 11), true),
    )

    val machinePlay3 = List(
      PlayPiece(pc('P'), Point(1, 11), true),
      PlayPiece(pc('E'), Point(2, 11), false),
      PlayPiece(pc('R'), Point(3, 11), true),
      PlayPiece(pc('U'), Point(4, 11), true),
    )

    val userPlay3 = List(
      PlayPiece(pc('E'), Point(1, 8), true),
      PlayPiece(pc('G'), Point(1, 9), true),
      PlayPiece(pc('I'), Point(1, 10), true),
      PlayPiece(pc('P'), Point(1, 11), false),
      PlayPiece(pc('T'), Point(1, 12), true),
      PlayPiece(pc('O'), Point(1, 13), true),
    )

    val result = for {
      game <- GameObj.mkGame(gameBase)

      (game1, _, _) <- game.addWordPlay(MachinePlayer, machinePlay1)
      machineScore1 = game1.miniState.lastPlayScore
      _ = machineScore1 shouldBe machinePlay1.count(_.moved)

      (game2, _, _) <- game1.addWordPlay(UserPlayer, userPlay1)
      userScore1 = game2.miniState.lastPlayScore
      _ = userScore1 shouldBe userPlay1.count(_.moved)

      (game3, _, _) <- game2.addWordPlay(MachinePlayer, machinePlay2)
      machineScore2 = game3.miniState.lastPlayScore
      _ = machineScore2 shouldBe machinePlay2.count(_.moved)

      (game4, _, _) <- game3.addWordPlay(UserPlayer, userPlay2)
      userScore2 = game4.miniState.lastPlayScore
      _ = userScore2 shouldBe userPlay2.count(_.moved)

      (game5, _, _) <- game4.addWordPlay(MachinePlayer, machinePlay3)
      machineScore3 = game5.miniState.lastPlayScore
      _ = machineScore3 shouldBe machinePlay2.count(_.moved)

      (game6, _, _) <- game5.addWordPlay(UserPlayer, userPlay3)
      userScore3 = game6.miniState.lastPlayScore
      _ = userScore3 shouldBe userPlay3.count(_.moved)
    } yield ()

    result match {
      case Success(_) => ()
      case Failure(ex) => fail(ex)
    }
  }


}
