package com.bolour.util

import java.sql.Timestamp
import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import slick.jdbc.{H2Profile, JdbcProfile, PostgresProfile, SQLiteProfile}
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.meta.MTable

object SlickUtil {
  val h2Driver = "org.h2.Driver"
  val sqliteDriver = "org.sqlite.JDBC"
  val postgresqlDriver = "org.postgresql.Driver"
  // TODO. Add postgres driver.

  def configuredDbAndProfile(dbConfigPath: String): (Database, JdbcProfile) = {
    val db = Database.forConfig(dbConfigPath)
    val config = ConfigFactory.load()
    val driver = config.getString(s"${dbConfigPath}.driver")
    val profile = driver match {
      case `h2Driver` => H2Profile
      case `sqliteDriver` => SQLiteProfile
      case `postgresqlDriver` => PostgresProfile
      case _ => throw new IllegalArgumentException(
        s"unsupported jdbc driver: ${driver} in configured db ${dbConfigPath}")
    }
    (db, profile)
  }

  def tableNames(db: Database): List[String] = {
    val future = db.run(MTable.getTables)
    val tables = Await.result(future, 10.second)
    tables.toList map {_.name.name}
  }

  class CustomColumnTypes(val profile: JdbcProfile) {
    import profile.api._ // Includes some implicits.
    implicit val javaTimeType =
      MappedColumnType.base[Instant, Timestamp](
        (instance: Instant) => new Timestamp(instance.getEpochSecond() * 1000),
        (timestamp: Timestamp) => Instant.ofEpochMilli(timestamp.getTime)
      )
  }
}
