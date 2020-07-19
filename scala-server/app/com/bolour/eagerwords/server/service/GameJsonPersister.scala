/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.service

import com.bolour.eagerwords.common.domain.GameState.GameState
import com.bolour.util.CommonUtil.{Email, ID}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Persister of games and players using JSON representations.
  */
trait GameJsonPersister {

  def migrate(): Future[Unit]

  def clearAllData(): Future[Unit] = {
    for {
      _ <- clearUserGameSettings()
      _ <- clearGames()
    } yield ()
  }

  def clearGames(): Future[Unit]
  def clearUserGameSettings(): Future[Unit]

  /**
    * Insert or update game data.
    *
    * @param gameId Unique id of the game.
    * @param playerId Unique internal user id of the player.
    * @param json Game data.
    */
  def saveGame(gameId: ID, playerId: Option[ID], firstSecond: Long, lastSecond: Long,
    state: GameState, scores: List[Int], json: String): Future[Unit]

  def findGameById(gameId: ID): Future[Option[String]]
  def deleteGame(gameId: ID): Future[Unit]
  def getUserGames(userId: ID, fromEpochSecond: Long, toEpochSecond: Long, maxGames: Int): Future[List[String]]

  /**
    * Get the unfinished games of a given user.
    *
    * @param userId The internal id of the user.
    * @return User's unfinished games as json strings.
    */
  def getUnfinishedUserGames(userId: ID): Future[List[String]];

  /**
    * Get a user's settings as json.
    */
  def getUserGameSettings(userId: ID): Future[Option[String]];

  /**
    * Same a user's settings as json.
    */
  def saveUserGameSettings(userId: String, json: String): Future[Unit];

}
