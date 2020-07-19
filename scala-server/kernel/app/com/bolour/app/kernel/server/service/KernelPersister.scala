/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.service

import com.bolour.app.kernel.server.domain.{EmailUser, Login, SignUp, User}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

// User is deprecated. Passwordless authentication use EmailUser.

/**
  * High-level persister interface for generic calls to applications.
  */
trait KernelPersister {

  def migrate(): Future[Unit]

  def clearAllData(): Future[Unit] = {
    for {
      _ <- clearEmailUsers()
      _ <- clearSignUps()
      _ <- clearLogins()
    } yield ()
  }

  def clearSignUps(): Future[Unit]

  /**
    * Add signUp information for a new email user.
    *
    * @param id Internal unique id in database.
    * @param email The user's email.
    * @param nickname The user's nickname.
    * @param clientId The client id to uniquely identify the client program (sent back to the client).
    * @param token The authentication token sent to the user's email for confirmation.
    * @param expiration Seconds from the epoch at which confirmation of signUp is timed out,
    *                   at which time the initial signUp request is invalidated and can be harvested.
    */
  def addSignUp(id: String, email: String, nickname: String, clientId: String, token: String, expiration: Long): Future[Unit]

  def findSignUpByClientId(clientId: String): Future[Option[SignUp]]

  def findSignUpByAuthEvidence(clientId: String, token: String): Future[Option[SignUp]]

  /**
    * If a signUp record exists for the email, remove it. Otherwise no-op.
    */
  def removeSignUp(email: String): Future[Unit]

  def clearEmailUsers(): Future[Unit]

  def findEmailUser(email: String): Future[Option[EmailUser]]

  def addEmailUser(id: String, email: String, nickname: String): Future[Unit]

  def clearLogins(): Future[Unit]

  def addLogin(id: String, email: String, clientId: String, token: String, confirmed: Boolean, expiration: Long, confirmExpiration: Long): Future[Unit] = ???

  def findLoginByEmail(email: String): Future[Option[Login]]

  def findLoginByAuthEvidence(clientId: String, token: String): Future[Option[Login]]

  def findLoginByClientId(clientId: String): Future[Option[Login]]

  def removeLogin(email: String): Future[Unit]

  /**
    * Use email as the unique identifier for updating the login record.
    * Must exist.
    *
    *
    */
  def updateLogin(email: String, confirmed: Boolean, expiration: Long): Future[Unit]

}
