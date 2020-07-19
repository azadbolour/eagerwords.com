/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.service

import com.bolour.eagerwords.common.domain.GameState.GameState
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import com.typesafe.config.Config
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.JdbcProfile
import com.bolour.util.CommonUtil.{Email, ID}
import com.bolour.app.util.server.SlickUtil.{configuredDbAndProfile, tableNames}
import com.bolour.eagerwords.common.domain.GameState
import com.bolour.eagerwords.common.domain.PlayerType._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

// TODO. Add indexes.

/**
  * Implementation of JSON persister using Slick.
  */
class GameJsonPersisterSlickImpl(val profile: JdbcProfile, db: Database) extends GameJsonPersister {

  import profile.api._

  val logger = LoggerFactory.getLogger(this.getClass)

  val gameTableName = "game"
  val userSettingsTableName = "user_settings"

  // Use large timeout to avoid internal error on overloaded machine.
  val timeout = 5.seconds

  def tableMap =
    Map(
      gameTableName -> gameRows,
      userSettingsTableName -> userSettingsRows
    )
  def allTableNames = tableMap.keySet.toList

  case class GameRow(id: ID, userId: Option[ID], firstSecond: Long, lastSecond: Long,
    state: String, userScore: Int, machineScore: Int, json: String)

  class GameTable(tag: Tag) extends Table[GameRow](tag, gameTableName) {
    def id = column[ID]("id", O.PrimaryKey)
    def userId = column[Option[ID]]("user_id")
    def firstSecond = column[Long]("first_second")
    def lastSecond = column[Long]("last_second")
    def state = column[String]("state")
    def userScore = column[Int]("user_score")
    def machineScore = column[Int]("machine_score")
    def json = column[String]("json")

    def * = (id, userId, firstSecond, lastSecond, state, userScore, machineScore, json).mapTo[GameRow]
  }

  def gameRows = TableQuery[GameTable]

  /**
    * Record of user's game settings.
    *
    * Even though the settings could be added to the user record,
    * we keep the user data structure pure and reusable by not polluting
    * it with settings.
    *
    * Of course, it may be argued that since many applications do have
    * user settings, and since the representation of settings
    * in the database is generic json, we might as well add settingsJson as
    * a field of the user record.
    *
    * Still, since the business logic of the different applications would use
    * different setting classes and is not generic, it seems cleaner to
    * just have a one-to-one correspondence between application classes
    * that need persistence and database tables.
    *
    * @param userId The internal id of the user.
    * @param json The user's game settings as a json string.
    */
  case class UserSettingsRow(userId: ID, json: String)

  class UserSettingsTable(tag: Tag) extends Table[UserSettingsRow](tag, userSettingsTableName) {
    def userId = column[ID]("user_id", O.PrimaryKey)
    def json = column[String]("json")

    def * = (userId, json).mapTo[UserSettingsRow]
  }

  def userSettingsRows = TableQuery[UserSettingsTable]

  override def migrate() = {
    val existingTableNames = tableNames(db)
    val neededTableNames = allTableNames diff existingTableNames
    val creates = neededTableNames map {name => tableMap(name).schema.create}
    db.run(DBIO.seq(creates:_*))
    // Await.result(future, timeout)
  }

  override def clearGames() = {
    for {
      num <- db.run(gameRows.delete)
    } yield ()
  }

  override def clearUserGameSettings(): Future[Unit] =
    for {
      num <- db.run(userSettingsRows.delete)
    } yield ()

  override def saveGame(gameId: ID, playerId: Option[ID], firstSecond: Long, lastSecond: Long,
    state: GameState, scores: List[Int], json: String) = {
    val userScore = scores(playerIndex(UserPlayer))
    val machineScore = scores(playerIndex(MachinePlayer))
    val gameRow = GameRow(gameId, playerId, firstSecond, lastSecond, state.toString, userScore, machineScore, json)
    val saveAction = gameRows.insertOrUpdate(gameRow)
    for {
      num <- db.run(saveAction)
    } yield ()
  }

  override def findGameById(gameId: ID) =  {
    val query = gameRows.filter {_.id === gameId }
    for {
      rows <- db.run(query.result)
      option = rows.headOption
    } yield option.map(_.json)
  }

  override def deleteGame(gameId: ID) = {
    val query = gameRows.filter {_.id === gameId }
    for {
      num <- db.run(query.delete)
    } yield ()
  }

  // TODO. Ignoring timing qualifications on user games. Getting all user games up to maxGames for now.
  // TODO. Add qualifications to getUserGames query.
  // TODO. The string is actually a json value. Define a type StringAsJson to make that clear.
  override def getUserGames(userId: String, fromEpochSecond: Long, toEpochSecond: Long,
    maxGames: Int): Future[List[String]] = {
    val query = for {
      gameRow <- gameRows if gameRow.userId === userId
    } yield gameRow;
    val q = query.take(maxGames).sortBy(_.firstSecond.desc).result.map(seq => seq.toList.map(row => row.json))
    db.run(q)
  }

  override def getUnfinishedUserGames(userId: ID): Future[List[String]] = {
    val query = for {
      gameRow <- gameRows if gameRow.userId === userId && gameRow.state =!= GameState.ENDED.toString
    } yield gameRow;
    val q = query.result.map(seq => seq.toList.map(row => row.json))
    db.run(q)
  }

  override def getUserGameSettings(userId: ID): Future[Option[String]] = {
    val query = userSettingsRows.filter {_.userId === userId }
    for {
      rows <- db.run(query.result)
      option = rows.headOption
    } yield option.map(_.json)
  }

  override def saveUserGameSettings(userId: String, json: String): Future[Unit] = {
    val row = UserSettingsRow(userId, json)
    val saveAction = userSettingsRows.insertOrUpdate(row)
    for {
      num <- db.run(saveAction)
    } yield ()

  }
}

object GameJsonPersisterSlickImpl {

  val dbConfigPrefix = "service.db"
  def confPath(pathInDb: String) =  s"${dbConfigPrefix}.${pathInDb}"

  def apply(dbName: String, config: Config): GameJsonPersisterSlickImpl = {
    val dbConfigPath = confPath(dbName)
    val (myDb, myProfile) = configuredDbAndProfile(dbConfigPath)
    new GameJsonPersisterSlickImpl(myProfile, myDb)
  }
}
