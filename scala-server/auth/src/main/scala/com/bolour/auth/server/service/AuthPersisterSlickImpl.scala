/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.auth.server.service

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import com.typesafe.config.Config
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.JdbcProfile
import org.slf4j.LoggerFactory

import com.bolour.util.CommonUtil.ID
import com.bolour.util.SlickUtil.{configuredDbAndProfile, tableNames}
import com.bolour.util.SymmetricCrypt
import com.bolour.auth.server.domain.{EmailUser, Login, SignUp}

// TODO. Add indexes as necessary.

/**
  * Implementation of generic persister using Slick.
  */
class AuthPersisterSlickImpl(val profile: JdbcProfile, db: Database, crypter: SymmetricCrypt) extends AuthPersister {

  import profile.api._

  val logger = LoggerFactory.getLogger(this.getClass)

  val signUpTableName = "signup"
  val loginTableName = "login"
  val emailUserTableName = "emailuser"

  // Use large timeout to avoid internal error on overloaded machine.
  val timeout = 5.seconds

  def tableMap = Map(
    signUpTableName -> signUpRows,
    loginTableName -> loginRows,
    emailUserTableName -> emailUserRows
  )
  def allTableNames = tableMap.keySet.toList

  def encryptEmail(plainEmail: String) =
    Future.fromTry(crypter.encrypt(plainEmail))
  def decryptEmail(cipherEmail: String) =
    Future.fromTry(crypter.decrypt(cipherEmail))

  case class EmailUserRow(id: ID, email: String, nickname: String)
  def emailUserRowToEmailUser(r: EmailUserRow): EmailUser =
    EmailUser(r.id, r.email, r.nickname)

  def emailUserRowToEmailUserFuture(r: EmailUserRow): Future[EmailUser] = {
    val cipherEmail = r.email
    val emailFuture = decryptEmail(cipherEmail)
    emailFuture.map(email => EmailUser(r.id, email, r.nickname))
  }

  class EmailUserTable(tag: Tag) extends Table[EmailUserRow](tag, emailUserTableName) {
    def id = column[ID]("id", O.PrimaryKey)
    def email = column[String]("email")
    def nickname = column[String]("nickname")

    def * = (id, email, nickname).mapTo[EmailUserRow]
    def emailIndex = index("email_user_email_index", email, unique = true)
  }

  def emailUserRows = TableQuery[EmailUserTable]

  case class SignUpRow(id: ID, email: String, nickname: String, clientId: String, token: String, expiration: Long)
  def signUpRowToSignUp(r: SignUpRow): SignUp =
    SignUp(r.email, r.nickname, r.clientId, r.token, r.expiration)

  def signUpRowToSignUpFuture(r: SignUpRow): Future[SignUp] = {
    val cipherEmail = r.email
    val emailFuture = decryptEmail(cipherEmail)
    emailFuture.map(email => SignUp(email, r.nickname, r.clientId, r.token, r.expiration))
  }

  class SignUpTable(tag: Tag) extends Table[SignUpRow](tag, signUpTableName) {
    def id = column[ID]("id", O.PrimaryKey)
    def email = column[String]("email")
    def nickname = column[String]("nickname")
    def clientId = column[String]("client_id")
    def token = column[String]("token")
    def expiration = column[Long]("expiration")

    def * = (id, email, nickname, clientId, token, expiration).mapTo[SignUpRow]
    def emailIndex = index("sign_up_email_index", email, unique = true)
    def clientIdIndex = index("sign_up_client_id_index", clientId, unique = true)
  }

  def signUpRows = TableQuery[SignUpTable]

  case class LoginRow(id: ID, email: String, clientId: String, token: String, confirmed: Boolean, expiration: Long,
    confirmExpiration: Long)
  def loginRowToLogin(r: LoginRow): Login =
    Login(r.email, r.clientId, r.token, r.confirmed, r.expiration, r.confirmExpiration)

  def loginRowToLoginFuture(r: LoginRow): Future[Login] = {
    val cipherEmail = r.email
    val emailFuture = decryptEmail(cipherEmail)
    emailFuture.map(email => Login(email, r.clientId, r.token, r.confirmed, r.expiration, r.confirmExpiration))
  }

  class LoginTable(tag: Tag) extends Table[LoginRow](tag, loginTableName) {
    def id = column[ID]("id", O.PrimaryKey)
    def email = column[String]("email")
    def clientId = column[String]("client_id")
    def token = column[String]("token")
    def confirmed = column[Boolean]("confirmed")
    def expiration = column[Long]("expiration")
    def confirmExpiration = column[Long]("confirm_expiration")

    def * = (id, email, clientId, token, confirmed, expiration, confirmExpiration).mapTo[LoginRow]
    def emailIndex = index("login_email_index", email, unique = true)
    def clientIdIndex = index("login_client_id_index", clientId, unique = true)
  }

  def loginRows = TableQuery[LoginTable]

  // TODO. Generic code. Abstract out of this class.
  override def migrate(): Future[Unit] = {
    val existingTableNames = tableNames(db)
    val neededTableNames = allTableNames diff existingTableNames
    val creates = neededTableNames map {name => tableMap(name).schema.create}
    db.run(DBIO.seq(creates:_*))
    // Await.result(future, timeout)
  }

  override def clearSignUps(): Future[Unit] = {
    for {
      num <- db.run(signUpRows.delete)
    } yield ()
  }

  override def addSignUp(id: String, email: String, nickname: String, clientId: String, token: String, expiration: Long): Future[Unit] = {
    for {
      cipherEmail <- encryptEmail(email)
      row = SignUpRow(id, cipherEmail, nickname, clientId, token, expiration)
      insert = signUpRows += row
      result <- db.run(insert).map(_ => ()) // TODO. 0 return??
    } yield result
  }

  override def findSignUpByAuthEvidence(clientId: String, token: String): Future[Option[SignUp]] = {
    val query = signUpRows.filter(row => row.clientId === clientId && row.token === token)
    for {
      rows <- db.run(query.result)
      signUpRows <- Future.sequence(rows.map(signUpRowToSignUpFuture))
    }
      yield signUpRows.headOption
  }

  override def findSignUpByClientId(clientId: String): Future[Option[SignUp]] = {
    val query = signUpRows.filter(row => row.clientId === clientId)
    for {
      rows <- db.run(query.result)
      signUpRows <- Future.sequence(rows.map(signUpRowToSignUpFuture))
    }
      yield signUpRows.headOption
  }

  override def removeSignUp(email: String): Future[Unit] = {
    for {
      cipherEmail <- encryptEmail(email)
      query = signUpRows.filter {_.email === cipherEmail }
      num <- db.run(query.delete)
    }
      yield ()
  }

  override def clearEmailUsers(): Future[Unit] = {
    for {
      num <- db.run(emailUserRows.delete)
    } yield ()
  }

  override def addEmailUser(id: String, email: String, nickname: String): Future[Unit] = {
    for {
      cipherEmail <- encryptEmail(email)
      row = EmailUserRow(id, cipherEmail, nickname)
      insert = emailUserRows += row
      result <- db.run(insert).map(_ => ()) // TODO. 0 return??
    } yield result
  }

  override def findEmailUser(email: String): Future[Option[EmailUser]] = {
    for {
      cipherEmail <- encryptEmail(email)
      query = emailUserRows.filter {_.email === cipherEmail }
      rows <- db.run(query.result)
      emailUserRows <- Future.sequence(rows.map(emailUserRowToEmailUserFuture))
    } yield emailUserRows.headOption
  }

  override def clearLogins(): Future[Unit] = {
    for {
      num <- db.run(loginRows.delete)
    } yield ()
  }

  override def addLogin(id: String, email: String, clientId: String, token: String,
    confirmed: Boolean, expiration: Long, confirmExpiration: Long): Future[Unit] = {
    for {
      cipherEmail <- encryptEmail(email)
      row = LoginRow(id, cipherEmail, clientId, token, confirmed, expiration, confirmExpiration)
      insert = loginRows += row
      result <- db.run(insert).map(_ => ()) // TODO. 0 return??
    } yield result
  }

  override def findLoginByEmail(email: String): Future[Option[Login]] = {
    for {
      cipherEmail <- encryptEmail(email)
      query = loginRows.filter {_.email === cipherEmail }
      rows <- db.run(query.result)
      loginRows <- Future.sequence(rows.map(loginRowToLoginFuture))
    }
      yield loginRows.headOption
  }

  override def findLoginByAuthEvidence(clientId: String, token: String): Future[Option[Login]] = {
    val query = loginRows.filter(row => row.clientId === clientId && row.token === token)
    for {
      rows <- db.run(query.result)
      loginRows <- Future.sequence(rows.map(loginRowToLoginFuture))
    }
      yield loginRows.headOption
  }

  override def findLoginByClientId(clientId: String): Future[Option[Login]] = {
    val query = loginRows.filter(row => row.clientId === clientId)
    for {
      rows <- db.run(query.result)
      loginRows <- Future.sequence(rows.map(loginRowToLoginFuture))
    }
      yield loginRows.headOption
  }

  override def removeLogin(email: String): Future[Unit] = {
    for {
      cipherEmail <- encryptEmail(email)
      query = loginRows.filter {_.email === cipherEmail }
      num <- db.run(query.delete)
    }
      yield ()
  }

  override def updateLogin(email: String, confirmed: Boolean, expiration: Long): Future[Unit] = {
    for {
      cipherEmail <- encryptEmail(email)
      query = loginRows.filter(_.email === cipherEmail).map(row => (row.confirmed, row.expiration))
      _ <- db.run(query.update(confirmed, expiration))
    }
      yield ()
  }

  override def removeSignedUpUser(email: String): Future[Unit] = {
    for {
      cipherEmail <- encryptEmail(email)
      signUpQuery = signUpRows.filter(_.email === cipherEmail)
      emailUserQuery = emailUserRows.filter(_.email === cipherEmail)
      loginQuery = loginRows.filter(_.email === cipherEmail)
      remover = for {
        _ <- signUpQuery.delete
        _ <- loginQuery.delete
        _ <- emailUserQuery.delete
      } yield ()
      transaction = remover.transactionally
      _ <- db.run(transaction)
    } yield ()
  }

}

// TODO. Duplicate code. Abstract out.
object AuthPersisterSlickImpl {

  val dbConfigPrefix = "service.db"
  def confPath(pathInDb: String) =  s"${dbConfigPrefix}.${pathInDb}"

  def apply(dbName: String, config: Config, secretService: SecretService): AuthPersisterSlickImpl = {
    val dbConfigPath = confPath(dbName)
    val (myDb, myProfile) = configuredDbAndProfile(dbConfigPath)
    val encryptionKey = secretService.getEncryptionKey
    val crypter = new SymmetricCrypt(encryptionKey)
    new AuthPersisterSlickImpl(myProfile, myDb, crypter)
  }
}
