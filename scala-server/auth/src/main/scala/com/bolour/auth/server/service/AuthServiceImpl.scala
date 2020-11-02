/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.auth.server.service

import javax.inject.Inject
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import com.typesafe.config.Config
import org.slf4j.LoggerFactory

import com.bolour.util.CommonUtil.optionToTry
import com.bolour.util.MiscUtil.stringId
import com.bolour.util.TimeUtil.nowSecs
import com.bolour.util.mail.{IMailService, SmtpMailServiceFactory}
import com.bolour.auth.server.domain.{EmailUser, Login, SignUp}
import com.bolour.auth.server.domain.AuthExceptions._

// TODO. Use Cats OptionT for Future[Option[T]) to simplify code involving Future and Option below.
// https://typelevel.org/cats/datatypes/optiont.html

/**
  * Implementation of generic application services.
  *
  * In the service layer API, the user is always identified by his external
  * unique identifier. In internal code below the service layer, the user is
  * always identified by his internal (database) identifier. That way the
  * external identifier, which is dependent on our particular authentication
  * provider, would not pollute our internal classes.
  *
  * Of course, it goes without saying that to obtain the internal identifier
  * of a user given his external identifier, some lower level calls need
  * to get the external identifier as a parameter. But that is the extent
  * of the use of external identifiers in lower-level APIs.
  *
  * @param config The configuration of the game service.
  * @param configuredDbName The name of the specific database to use as configured in application.conf, if given.
  *                         Use "defaultDb" if none given.
  */
class AuthServiceImpl @Inject() (config: Config, secretService: SecretService, configuredDbName: Option[String]) extends AuthService {

  val logger = LoggerFactory.getLogger(this.getClass)

  val dbConfigPrefix = AuthService.confPath("db")
  val dbName = configuredDbName.getOrElse("defaultDb");
  val defaultDbPath = s"${dbConfigPrefix}.${dbName}"

  val mailConfigPrefix = AuthService.confPath("email");

  // TODO. Validate service method parameters.
  // To the extent validation code is implementation-independent,
  // implement in the base trait.

  val defaultDb = config.getString(defaultDbPath)
  val persister: AuthPersister = AuthPersisterSlickImpl(defaultDb, config, secretService)
  val mailConfig: Config = config.getConfig(mailConfigPrefix);
  // val emailService: IMailService = new MockSmtpMailService(mailConfig)
  val emailService: IMailService = SmtpMailServiceFactory.create(mailConfig)

  val mockEmail = secretService.getTestingEmail
  val mockToken = secretService.getTestingToken

  migrate()

  override def migrate() = {
    // TODO. Proper migration. This one is for testing only.
    // Version the server - and create an upgrade function for each new version.
    // Keep last upgraded version in the database.
    for /* Future */ {
    _ <- persister.migrate()
    //      maybeSeedPlayer <- persister.findPlayerByName(seedPlayerName)
    //      _ <- maybeSeedPlayer match {
    //        case None => persister.savePlayer(seedPlayer)
    //        case _ => Success(())
    //      }
    } yield ()
  }

  override def reset(): Future[Unit] = {
    persister.clearAllData()
  }

  override def findEmailUser(email: String): Future[Option[EmailUser]] = {
    persister.findEmailUser(email)
  }

  override def getEmailUser(email: String): Future[EmailUser] = {
    for {
      optEmailUser <- findEmailUser(email)
      triedEmailUser = optionToTry(optEmailUser) {NotSignedUpException(email)}
      emailUser <- Future.fromTry(triedEmailUser)
    } yield emailUser
  }

  override def removeSignedUpUser(email: String): Future[Unit] = {
    for {
      ouser <- findEmailUser(email)
      _ <- ouser match {
        case None => Future.failed(MissingUserException(email))
        case Some(user) => persister.removeSignedUpUser(email)
      }
    } yield ()
  }

  /**
    * Generate an authentication token, except for an integration test client
    * whose mock token is fixed.
    *
    * @param email The user's email - determines if the user is an integration test client.
    */
  private def mkToken(email: String): String =
    if (email == mockEmail) mockToken else stringId().take(6)

  /**
    * Send the confirmation email except for an integration test client, whose token
    * is fixed and will be known to the integration test.
    *
    * @param email The email address - mockEmail indicates an integration test client.
    * @param subject The email subject.
    * @param token The authentication token.
    */
  private def sendRealMail(email: String, subject: String, token: String): Unit =
    if (email == mockEmail) ()
    else emailService.sendMail(email, subject, token)

  override def initSignUp(email: String, nickname: String): Future[String] = {
    val clientId = stringId()
    val token = mkToken(email)
    val signUpId = stringId()
    val expiration = nowSecs + confirmationTimeout;
    val subject = "eagerwords sign-up validation"

    for {
      optEmailUser <- persister.findEmailUser(email)
      _ <- optEmailUser match {
        case Some(_) => Future.failed(AlreadySignedUpException())
        case None => persister.removeSignUp(email)
      }
      _ <- persister.addSignUp(signUpId, email, nickname, clientId, token, expiration)
      // TODO. Use asynchronous email sender.
      _ = sendRealMail(email, subject, token)
    }
      yield clientId
  }

  override def confirmSignUp(clientId: String, token: String): Future[Unit] = {
    for {
      optSignUp <- persister.findSignUpByClientId(clientId)
      _ <- optSignUp match {
        case None => Future.failed(SignUpConfirmationWithoutInitializationException())
        case Some(signUp) => for {
            optEmailUser <- persister.findEmailUser(signUp.email)
            _ <- optEmailUser match {
              case None => confirmSignUpInternal(signUp, token)
              case Some(_) => Future.failed(AlreadySignedUpException())
            }
          } yield ()
      }
    } yield ()
  }

  private def confirmSignUpInternal(signUp: SignUp, providedToken: String): Future[Unit] = {
    val loginId = stringId()
    val loginExpiration = nowSecs + (2 * 60 * 60) // Initial login expiration. TODO. Constant.
    val email = signUp.email
    val clientId = signUp.clientId
    val token = signUp.token
    val confirmed = true
    val confirmationExpiration = nowSecs - 1 // Already confirmed.

    if (providedToken != token)
      Future.failed(ConfirmationTokenMismatchException())
    else if (signUp.expiration <= nowSecs)
      Future.failed(ConfirmationTimeoutException())
    else
      // TODO. URGENT. Transaction.
      for {
        _ <- persister.removeSignUp(signUp.email)
        _ <- persister.addEmailUser(stringId(), signUp.email, signUp.nickname)
        // Login the user - using the the clientId and token used for signup.
        _ <- persister.addLogin(
               loginId, email, clientId, token, confirmed, loginExpiration, confirmationExpiration)
      } yield ()
  }

  override def initLogin(email: String, timeout: Long): Future[String] = {
    val clientId = stringId()
    val token = mkToken(email)
    val loginId = stringId()
    val now = nowSecs
    val confirmExpiration = now + confirmationTimeout;
    val subject = "eagerwords login validation"

    for {
      _ <- if (timeout > loginTimeout)
             Future.failed(MaxTimeoutExceededException(timeout, loginTimeout))
           else Future.successful(())
      expiration = now + timeout
      optEmailUser <- persister.findEmailUser(email)
      _ <- optEmailUser match {
        case None => Future.failed(NotSignedUpException(email))
        case Some(_) => Future.successful(())
      }
      optLogin <- persister.findLoginByEmail(email)
      _ <- optLogin match {
        case None => Future.successful(())
        case Some(_) =>
          // The user may have lost their browser session.
          // So must cancel the earlier login and re-init login.
          persister.removeLogin(email)
      }
      _ <- persister.addLogin(loginId, email, clientId, token, false, expiration, confirmExpiration)
      // TODO. Use asynchronous email sender.
      _ = if (email == mockEmail) ()
          else sendRealMail(email, subject, token)
    }
      yield clientId
  }

  override def confirmLogin(clientId: String, token: String): Future[String] = {
    for {
      optLogin <- persister.findLoginByClientId(clientId)
      nickname <- optLogin match {
          case None => Future.failed(LoginConfirmationWithoutInitializationException())
          case Some(login) => confirmLoginInternal(login, token)
        }
    } yield (nickname)
  }

  private def confirmLoginInternal(login: Login, providedToken: String): Future[String] = {
    val email = login.email;
    if (login.confirmed)
      Future.failed(AlreadyConfirmedException())
    else if (providedToken != login.token)
      Future.failed(ConfirmationTokenMismatchException())
    else if (login.confirmExpiration <= nowSecs)
      Future.failed(ConfirmationTimeoutException())
    else
      for {
        optEmailUser <- persister.findEmailUser(email)
        nickname <- optEmailUser match {
            // TODO. This would be an internal error! The login record should be removed to restore sanity.
            case None => Future.failed(NotSignedUpException(email))
            case Some(emailUser) => Future.successful(emailUser.nickname)
          }
        _ <- persister.updateLogin(login.email, true, nowSecs + loginTimeout)
      } yield (nickname)
  }

  override def isLoggedIn(clientId: String, token: String): Future[Boolean] = {
    for {
      optLogin <- persister.findLoginByAuthEvidence(clientId, token)
      _ = logger.info(s"isLoggedIn - optLogin: ${optLogin}")
      loggedIn <- optLogin match {
        case None => Future.successful(false)
        case Some(login) =>
          val is = login.isLoggedIn
          logger.info(s"nowSecs: ${nowSecs}, expiration: ${login.expiration}, confirmed: ${login.confirmed}, isLoggedIn: ${is}")
          Future.successful(login.isLoggedIn)
      }
    } yield loggedIn
  }

  def checkLogin(clientId: String, token: String): Future[Login] = {
    for {
      optLogin <- persister.findLoginByAuthEvidence(clientId, token)
      login <- optLogin match {
        // TODO. Define and use LoggedOutException.
        case None => Future.failed(MissingAuthEvidenceException())
        case Some(login) => Future.successful(login)
      }
    } yield login
  }

  override def logout(clientId: String, token: String): Future[Unit] = {
    for {
      optLogin <- persister.findLoginByAuthEvidence(clientId, token)
      _ <- optLogin match {
        case None => Future.failed(MissingAuthEvidenceException())
        case Some(login) =>
          // Just remove the login whether or not it is still valid.
          persister.removeLogin(login.email)
      }
    } yield ()
  }

}


