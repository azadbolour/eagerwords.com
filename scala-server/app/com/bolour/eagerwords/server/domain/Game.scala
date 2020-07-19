/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.domain

import com.bolour.eagerwords.common.domain.PlayerType._
import com.bolour.eagerwords.server.domain.GameExceptions.{InvalidCrosswordsException, MalformedPlayException}
import com.bolour.language.domain.WordDictionary
import com.bolour.eagerwords.common.domain.{PlayerType, _}
import GameState._
import com.bolour.eagerwords.server.service.GameServiceImpl.mkPieceProvider
import com.bolour.plane.domain.Point
import org.slf4j.LoggerFactory
import com.bolour.util.TimeUtil._

import scala.util.{Success, Try}

/**
  * The game.
  *
  * @param gameBase Data about the game that is conceptually immutable.
  * @param board The game's board.
  * @param trays The user and machine trays of pieces.
  * @param pieceProvider The factory for generating new pieces.
  * @param playNumber The ordinal number of the current play.
  * @param playTurn Whose play is it? User or machine.
  * @param lastPlayScore Score of the last play.
  * @param numSuccessivePasses Number of successive passes (if maxed out game ends).
  * @param scores The total scores of each player.
  * @param plays List of plays in the game so far.
  */
case class Game(
  gameBase: GameBase,
  board: Board,
  trays: List[Tray],
  pieceProvider: PieceProvider,
  playNumber: Int,
  playTurn: PlayerType,
  lastPlayScore: Int,
  numSuccessivePasses: Int,
  scores: List[Int],
  plays: Vector[Play],
  state: GameState = GameState.RUNNING,
  lastSecond: Long = nowSecs
) {
  import GameObj.MaxSuccessivePasses

  val logger = LoggerFactory.getLogger(this.getClass)
  val scorer = gameBase.scorer

  def isRunning = GameState.isRunning(state)
  def isCompleted = GameState.isCompleted(state)

  def passesMaxedOut: Boolean = numSuccessivePasses == MaxSuccessivePasses

  def isGuestGame: Boolean = gameBase.playerId.isEmpty

  def miniState: GameMiniState =
    GameMiniState(lastPlayScore, scores, passesMaxedOut || board.isFilled)

  def stopInfo: StopInfo = StopInfo(numSuccessivePasses, board.isFilled)

  // TODO. May fail. So return a Try for consistency.
  // For now nothing to do.
  def stop(): Game = {
    this
  }

  def summary(): GameSummary = GameSummary(stopInfo)

  def end(): Game = this.copy(
    lastSecond = nowSecs,
    state = GameState.ENDED
  )

  def suspend(): Game = this.copy(
    lastSecond = nowSecs,
    state = GameState.SUSPENDED
  )

  def resign(): Game = this.copy(
    lastSecond = nowSecs,
    state = GameState.RESIGNED
  )

  /**
    * Add the next play to the game, updating the board and returning
    * pieces to replace those used in the play.
    *
    * @param playerType User or machine.
    * @param playPieces Ordered list of pieces used in the play and their locations.
    *                   Includes all pieces that form the played word, and for each
    *                   an indication of whether they were moved in this play.
    * @param deadPointFinder Function to look for dead points after this play -
    *                        points that cannot possibly  be filled.
    * @return The updated game, a list of replacement pieces,
    *         and a list of dead points detected after the play is made.
    */
  def addWordPlay(playerType: PlayerType, playPieces: List[PlayPiece],
    deadPointFinder: Board => (Board, List[Point]) = GameObj.noDeads): Try[(Game, List[Piece], List[Point])] = {
    for {
      _ <- if (playerType == UserPlayer) validatePlay(playerType, playPieces) else Success(())
      movedPiecePoints = playPieces filter { _.moved } map { _.piecePoint }
      score = computePlayScore(playPieces)
      (playedGame, refills) <- addGoodWordPlay(playerType, movedPiecePoints, score)
      (newBoard, deadPoints) = deadPointFinder(playedGame.board)
      wordPlay = Play.mkWordPlay(playedGame.playNumber, playerType, playedGame.scores, playPieces, refills, deadPoints)
      playsPlus = plays :+ wordPlay
      newGame = playedGame.copy(board = newBoard, plays = playsPlus, lastSecond = nowSecs)
    } yield (newGame, refills, deadPoints)
  }

  def setDeadPoints(deadPoints: List[Point]): Game = {
    val newBoard = board.setBlackPoints(deadPoints)
    this.copy(board = newBoard, lastSecond = nowSecs)
  }

  def tray(playerType: PlayerType): Tray = trays(playerIndex(playerType))

  def computePlayScore(playPieces: List[PlayPiece]): Int = {
    scorer.scorePlay(playPieces)
  }

  /**
    * Add a validated play to the game and return replacements for the played pieces.
    */
  private def addGoodWordPlay(playerType: PlayerType, piecePoints: List[PiecePoint], score: Int): Try[(Game, List[Piece])] = {
    val newBoard = board.setPiecePoints(piecePoints)
    val usedPieces = piecePoints map { _.piece }
    val succPasses = if (score > 0) 0 else numSuccessivePasses + 1
    val ind = playerIndex(playerType)
    for {
      (nextProvider, newPieces) <- pieceProvider.takePieces(usedPieces.length)
      newTrays = trays.updated(ind, trays(ind).replacePieces(usedPieces, newPieces))
      newScores = scores.updated(ind, scores(ind) + score)
      nextType = nextPlayerType(playerType)
      newGame = Game(gameBase, newBoard, newTrays, nextProvider, playNumber + 1, nextType, score, succPasses, newScores, plays)
    } yield ((newGame, newPieces))
  }

  def addSwapPlay(piece: Piece, playerType: PlayerType): Try[(Game, Piece)] = {
    val succPasses = numSuccessivePasses + 1
    val trayIndex = PlayerType.playerIndex(playerType)
    val tray = trays(trayIndex)
    for {
      tray1 <- tray.removePiece(piece)
      (pieceProvide1, newPiece) <- pieceProvider.swapOne(piece)
      tray2 <- tray1.addPiece(newPiece)
      trays2 = trays.updated(playerIndex(playerType), tray2)
      newGame = this.copy(trays = trays2, pieceProvider = pieceProvide1, playNumber = playNumber + 1, numSuccessivePasses = succPasses, lastPlayScore = 0)
      swapPlay = Play.mkSwapPlay(newGame.playNumber, playerType, newGame.scores, piece, newPiece)
      playsPlus = plays :+ swapPlay
      newGamePlus = newGame.copy(plays = playsPlus, lastSecond = nowSecs)
    } yield (newGamePlus, newPiece)
  }

  /**
    * Check that cross words generated by this play are valid words in the dictionary.
    */
  def checkCrossWords(playPieces: List[PlayPiece], theDictionary: WordDictionary): Try[Unit] = {
    val theBoard = board
    val theTray = trays(playerIndex(UserPlayer))
    Try {
      val playStrip = board.stripOfPlay(playPieces)
      val stripMatcher = new StripMatcher {
        override def dictionary = theDictionary
        override def board = theBoard
        override def tray = theTray
      }
      val word = PlayPieceObj.playPiecesToWord(playPieces)
      val crosswords = stripMatcher.crossings(playStrip, word)
      val invalidCrosswords = crosswords.filter { w => !(theDictionary hasWord w) }
      if (!invalidCrosswords.isEmpty)
        throw new InvalidCrosswordsException(gameBase.gameParams.languageCode, invalidCrosswords)
    }
  }

  private def validatePlay(playerType: PlayerType, playPieces: List[PlayPiece]): Try[Unit] = {
    if (playerType == MachinePlayer)
      return Success(())

    for {
      _ <- checkPlayLineInBounds(playPieces)
      _ <- checkPlayAnchored(playPieces)
      _ <- checkContiguousPlay(playPieces)
      _ <- checkMoveDestinationsEmpty(playPieces)
      _ <- checkUnmovedPlayPositions(playPieces)
      _ <- checkMoveTrayPieces(playPieces)
    } yield ()
  }

  // TODO. Implement all play validation checks.
  // Most of the validations are also done in the associated web ui.
  // To allow unrelated client code to use the API, however, these checks
  // need to be implemented.

  def inBounds(coordinate: Int): Boolean =
    coordinate >= 0 && coordinate < gameBase.gameParams.dimension

  def inBounds(point: Point): Boolean = {
    inBounds(point.row) && inBounds(point.col)
  }
  /**
    * Check that play locations are within the board's boundaries.
    */
  private def checkPlayLineInBounds(playPieces: List[PlayPiece]): Try[Unit] = Try {
    val points = playPieces.map {_.point}
    if (!points.forall(inBounds(_)))
      throw MalformedPlayException("play points out of bounds")
  }

  // private def checkFirstPlayCentered(playPieces: List[PlayPiece]): Try[Unit] = Success(())

  /**
    * Check that the play is anchored - except for the first play.
    * TODO. Implement validation. Done in UI as well. So OK to defer.
    * Note. Anchored means the play includes at least one existing board piece,
    * or that the play is parallel. However, parallel plays will likely be removed.
    */
  private def checkPlayAnchored(playPieces: List[PlayPiece]): Try[Unit] = Success(())

  /**
    * Check that play for for a contiguous strip on the board.
    */
  private def checkContiguousPlay(playPieces: List[PlayPiece]): Try[Unit] = Try {
    val points = playPieces.map {_.point}
    if (points.length != points.distinct.length)
      throw MalformedPlayException("duplicate play points")

    val rows = points.map { _.row }
    val cols = points.map { _.col }

    // TODO. Move to util.
    def same[A](values: List[A]): Boolean =
      if (values.length < 2) true else values.drop(1).forall(_ == values.head)
    if (!same(rows) && !same(cols))
      throw MalformedPlayException("non-contiguous play points")
  }

  /**
    * Check that the destinations of the moved play pieces are empty.
    * TODO. Implement validation. Done in UI as well. So OK to defer.
    */
  private def checkMoveDestinationsEmpty(playPieces: List[PlayPiece]): Try[Unit] = Success(())

  /**
    * Check that the play pieces that are not moved and are supposed to exist on the board
    * are in fact there and contain the pieces indicated in the play.
    * TODO. Implement validation. Done in UI as well. So OK to defer.
    */
  private def checkUnmovedPlayPositions(playPieces: List[PlayPiece]): Try[Unit] = Success(())

  /**
    * Check that the moved pieces in this play are in fact user tray pieces.
    */
  private def checkMoveTrayPieces(playPieces: List[PlayPiece]): Try[Unit] = Success(())

  def sanityCheck: Try[Unit] = Try {
    val boardPieces = board.piecePoints.map { _.piece }
    val userTrayPieces = trays(playerIndex(UserPlayer)).pieces
    val machineTrayPieces = trays(playerIndex(MachinePlayer)).pieces

    val boardPieceIds = boardPieces.map { _.id }
    val userPieceIds = userTrayPieces.map { _.id }
    val machinePieceIds = machineTrayPieces.map { _.id }

    val allIds = boardPieceIds ++ userPieceIds ++ machinePieceIds
    val dups = allIds diff allIds.distinct
    if (dups.nonEmpty) {
      val message = s"duplicate ids in game state: ${dups}"
      logger.error(message)
      throw new IllegalStateException(message)
    }
  }
}

object GameObj {

  def MaxSuccessivePasses = 10

  def mkGame(gameBase: GameBase): Try[Game] = {

    val initPieces = gameBase.initPieces
    val board = Board(gameBase.gameParams.dimension, initPieces.boardPiecePoints)

    val pieceProvider: PieceProvider = mkPieceProvider(gameBase.gameParams.pieceProviderType)
    val lastPlayScore = 0

    for {
      (userTray, pieceGen1) <- mkTray(gameBase.gameParams.trayCapacity, initPieces.userPieces, pieceProvider)
      (machineTray, pieceGen2) <- mkTray(gameBase.gameParams.trayCapacity, initPieces.machinePieces, pieceGen1)
    }
      yield Game(gameBase, board, List(userTray, machineTray), pieceGen2, 0, UserPlayer, lastPlayScore, 0, List(0, 0), Vector.empty)
  }

  def mkTray(capacity: Int, initPieces: List[Piece], pieceGen: PieceProvider): Try[(Tray, PieceProvider)] = {
    if (initPieces.length >= capacity)
      return Success((Tray(capacity, initPieces.take(capacity).toVector), pieceGen))

    for {
      (nextGen, restPieces) <- pieceGen.takePieces(capacity - initPieces.length)
      pieces = initPieces ++ restPieces
    } yield (Tray(capacity, pieces.toVector), nextGen)
  }

  def noDeads(board: Board): (Board, List[Point]) = (board, Nil)

}
