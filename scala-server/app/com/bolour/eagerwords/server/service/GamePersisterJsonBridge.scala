/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.server.service

import com.bolour.eagerwords.common.domain.UserGameSettings
import com.bolour.app.kernel.server.domain.KernelExceptions.InternalAppException
import com.bolour.eagerwords.server.domain.Game
import com.bolour.util.CommonUtil.ID
import com.bolour.util.VersionStamped
import io.circe.generic.auto._
import io.circe.parser.{decode, _}
import io.circe.syntax._
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Implementation of the hig-level game persister interface using a JSON persister.
  * Converts objects to their JSON representations and calls lower-level JSON
  * persister to persist them.
  *
  * @param jsonPersister The specific lower-level JSON persister to use.
  * @param version The server version - in case JSON representations change over time.
  */
class GamePersisterJsonBridge(jsonPersister: GameJsonPersister, version: Int) extends GamePersister {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def migrate() = jsonPersister.migrate()

  override def clearGames() = jsonPersister.clearGames()
  override def clearUserGameSettings() = jsonPersister.clearUserGameSettings()

  override def saveGame(game: Game): Future[Unit] = {
    val versionedGameData = VersionStamped[Game](version, game)
    val gameId = game.gameBase.gameId
    val playerId = game.gameBase.playerId
    val firstSecond = game.gameBase.firstSecond
    val lastSecond = game.lastSecond
    val state = game.state
    val scores = game.scores
    val json = versionedGameData.asJson.toString
    jsonPersister.saveGame(gameId, playerId, firstSecond, lastSecond, state, scores, json)
  }

  override def findGameById(gameId: ID) = {
    for {
      ojson <- jsonPersister.findGameById(gameId)
      odata = ojson map { json =>
        val parseResult = decode[VersionStamped[Game]](json)
        parseResult match {
          case Right(versionedData) => versionedData.data
          case Left(error) => throw InternalAppException(s"error in decoding game data - ${error.getMessage}", error)
        }
      }
    } yield odata
  }

  override def deleteGame(gameId: ID): Future[Unit] = jsonPersister.deleteGame(gameId)

  override def removeAllUserGameRelatedInfo(userId: ID): Future[Unit] =
    jsonPersister.removeAllUserGameRelatedInfo(userId)

  override def getUnfinishedUserGames(userId: ID): Future[List[Game]] = {
    val futureJsons = jsonPersister.getUnfinishedUserGames(userId)
    decodeJsonGames(futureJsons)
  }

  override def getUserGames(userId: ID, fromEpochSecond: Long, toEpochSecond: Long, maxGames: Int): Future[List[Game]] = {
    val futureJsons = jsonPersister.getUserGames(userId, fromEpochSecond, toEpochSecond, maxGames)
    decodeJsonGames(futureJsons)
  }

  override def getUserGameSettings(userId: ID): Future[Option[UserGameSettings]] = {
    for /* Future */ {
    jsonOption <- jsonPersister.getUserGameSettings(userId)
    optionEither = jsonOption.map(decode[VersionStamped[UserGameSettings]])
    optionVersionedSettings = optionEither match {
        case None => None
        case Some(Right(settings)) => Some(settings)
        case Some(Left(error)) => throw InternalAppException(s"error in decoding settings - $error", error)
      }
    } yield optionVersionedSettings.map(_.data)
  }

  override def saveUserGameSettings(userId: String, settings: UserGameSettings): Future[Unit] = {
    val versionedData = VersionStamped[UserGameSettings](version, settings)
    val json = versionedData.asJson.toString
    jsonPersister.saveUserGameSettings(userId, json);
  }

  // More elegant error handling??
  private def decodeJsonGames(futureJsons: Future[List[String]]): Future[List[Game]] = {
    for {
      jsons <- futureJsons
      gameEithers = jsons.map(decode[VersionStamped[Game]](_))
      versionedGames = gameEithers collect { case Right(game) => game }
      errors = gameEithers collect { case Left(error) => error }
    } yield errors match {
      case Nil => versionedGames.map(_.data)
      case error::_ => throw InternalAppException(s"error in decoding game - $error", error)
    }
  }
}
