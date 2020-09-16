/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.service

import com.bolour.eagerwords.common.domain.UserGameSettings
import com.bolour.eagerwords.server.domain.Game
import com.bolour.util.CommonUtil.ID

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * High-level persister interface for game application objects.
  */
trait GamePersister {

  def migrate(): Future[Unit]

  def clearAllData(): Future[Unit] = {
    for {
      _ <- clearUserGameSettings()
      _ <- clearGames()
    } yield ()
  }

  def clearGames(): Future[Unit]
  def clearUserGameSettings(): Future[Unit]

  def saveGame(game: Game): Future[Unit]
  def findGameById(gameId: ID): Future[Option[Game]]
  def deleteGame(gameId: ID): Future[Unit]

  /**
    * Get the user's games overlapping a given time interval.
    *
    * @param userId The internal id of the user.
    * @param fromEpochSecond Inclusive lower bound of the required interval in epoch seconds.
    * @param toEpochSecond Exclusive upper bound of the the required interval in epoch seconds.
    * @param maxGames Limit on the number of (most recent) games to be retrieved.
    * @return The user's game overlapping the given time interval.
    */
  def getUserGames(userId: ID, fromEpochSecond: Long, toEpochSecond: Long, maxGames: Int): Future[List[Game]]

  /**
    * Get unfinished games for a user.
    *
    * @param userId The internal id of the user.
    * @return List of the user's unfinished games.
    */
  def getUnfinishedUserGames(userId: ID): Future[List[Game]]

  /**
    * Get a user's game settings if saved.
    */
  def getUserGameSettings(userId: ID): Future[Option[UserGameSettings]]

  /**
    * Save a user's game settings.
    */
  def saveUserGameSettings(userId: ID, settings: UserGameSettings): Future[Unit]
}
