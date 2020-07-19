/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.service

import com.bolour.util.CommonUtil.ID
import com.bolour.eagerwords.server.domain.{Game, GameBase}
import com.bolour.eagerwords.common.domain._
import com.bolour.plane.domain.Point

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.Try

/**
  * Trait representing the service layer of the application.
  *
  * All methods return Try of the data needed for the happy path to represent
  * possible errors. We won't repeat this for every method.
  */
trait GameService {

  // TODO. Add parameter validation is common to all implementations.

  /**
    * Migrate the persistent data.
    */
  def migrate(): Future[Unit]

  /**
    * Clear out the persistent data (for testing).
    */
  def reset(): Future[Unit]

  /**
    * Save the game settings for a given user.
    */
  def saveUserGameSettings(email: String, settings: GameSettings): Future[Unit];

  /**
    * Get the game settings of a user.
    */
  def getUserGameSettings(email: String): Future[Option[GameSettings]];

  /**
    * Start a new game.
    *
    * @param gameParams Specification of the new game.
    * @param initPieces Initial pieces on the board - for testing.
    * @param maybeEmail The user's email as option.
    * @return The created game.
    */
  def startGame(gameParams: GameParams, initPieces: InitPieces, maybeEmail: Option[String]): Future[Game]

  def checkGameOwner(gameId: String, email: String): Future[Unit]

  /**
    * Commit a user play.
    *
    * @param gameId Unique id of the game.
    * @param playPieces The specification of the played word: a sequence of play pieces,
    *                   one for each letter of the word, designating the letter, its location,
    *                   and whether it was moved from the tray in this play.
    * @return Tuple (mini state, replacement pieces, dead locations).
    *         The mini state is minimal information about the updated game needed by clients.
    *         Replacement pieces fill in for the pieces used in the play.
    *         Dead locations are board locations that were detected as impossible to fill
    *         after this play.
    */
  def commitPlay(gameId: ID, playPieces: List[PlayPiece]): Future[(GameMiniState, List[Piece], List[Point])]

  /**
    * Make a machine play.
    *
    * See commitPlay for parameters and return.
    */
  def machinePlay(gameId: ID): Future[(GameMiniState, List[PlayPiece], List[Point])]

  /**
    * Swap a user piece.
    *
    * Note. Swapping of machine piece is an implementation detail of machinePlay.
    *
    * @param gameId Unique id of the game.
    * @param piece Piece being swapped.
    * @return Minimal state of the game after the swap and the new piece.
    */
  def swapPiece(gameId: ID, piece: Piece): Future[(GameMiniState, Piece)]

  /**
    * End the game.
    *
    * @param gameId Unique id of the game.
    * @return Summary of the game.
    */
  def endGame(gameId: ID): Future[GameSummary]

  def suspendGame(gameId: ID): Future[Unit]

  /**
    * Resume a suspended game. If not suspended this is basically a no-op.
    */
  def resumeGame(gameId: ID): Future[Game]
  def cancelGame(gameId: ID): Future[Unit]
  def resignGame(gameId: ID): Future[Unit]

  def getUnfinishedUserGames(email: String): Future[List[Game]]

  /**
    * Get the games of a user within a given time range.
    *
    * @param email The external identifier of the user.
    * @param fromEpochSecond Inclusive lower time bound.
    * @param toEpochSecond Exclusive upper time bound.
    * @param maxGames Limit on the number of games to return.
    *                 The most recent maxGames (or the configured maxGetGames value,
    *                 whichever is smaller) will be returned.
    * @return List of Games for the user within the given range.
    */
  def getUserGames(
    email: String,
    fromEpochSecond: Long,
    toEpochSecond: Long,
    maxGames: Int
  ): Future[List[Game]]

  /**
    * Find a game.
    */
  def findGameById(gameId: ID): Future[Option[Game]]

  /**
    * Harvest long-running games - considering them and abandoned.
    */
  def timeoutLongRunningGames(): Future[Unit]

}

object GameService {
  val serviceConfigPrefix = "service"
  def confPath(pathInService: String) =  s"${serviceConfigPrefix}.${pathInService}"

  val maxActiveGamesConfigPath = confPath("maxActiveGames")
  val maxGameIdleMinutesConfigPath = confPath("maxGameIdleMinutes")
  val maxGetGamesConfigPath = confPath("maxGetGames")
  val languageCodesConfigPath = confPath("languageCodes")
}
