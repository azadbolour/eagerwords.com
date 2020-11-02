/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.auth.server.service

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.bolour.auth.server.domain.{EmailUser, Login, SignUp}

/**
  * High-level persister interface for generic calls to applications.
  */
trait AuthPersister {

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

  /**
   * Find a signup record by using the unique id of a requesting client.
   *
   * @param clientId The unique id of the client program assigned by this service during the
   *                 signup/signin process.
   * @return The user's signup information, if the clientId exists as an authorized client.
   */
  def findSignUpByClientId(clientId: String): Future[Option[SignUp]]

  /**
   * Find a signup record by using evidence of recent signup/signin.
   *
   * @param clientId The unique id sent to the client program by this service during the
   *                 signup/signin process.
   * @param token The login token sent by email to the user.
   * @return The user's signup information if the user is still logged in.
   */
  def findSignUpByAuthEvidence(clientId: String, token: String): Future[Option[SignUp]]

  /**
    * If a signUp record exists for the email, remove it. Otherwise no-op.
    */
  def removeSignUp(email: String): Future[Unit]

  /**
   * Delete all email users. Used for testing.
   */
  def clearEmailUsers(): Future[Unit]

  /**
   * Find user information by user's email.
   *
   * @param email The user's email.
   * @return The registration data about the email user.
   */
  def findEmailUser(email: String): Future[Option[EmailUser]]

  /**
   * Insert a new user into the database.
   *
   * @param id The internal database id of the user.
   * @param email The user's email.
   * @param nickname The user's email.
   */
  def addEmailUser(id: String, email: String, nickname: String): Future[Unit]

  /**
   * Remove all login records. Used in testing.
   */
  def clearLogins(): Future[Unit]

  /**
   * Add a login record.
   *
   * @param id The internal database login id.
   * @param email The user's email.
   * @param clientId The unique id sent to the calling client program.
   * @param token The authentication token sent to the user.
   * @param confirmed Was the login confirmed?
   * @param expiration Expiration date (in epoch seconds) of this login.
   * @param confirmExpiration Time (in epoch seconds) by which confirmation of login must be
   *                          received from the user.
   */
  def addLogin(id: String, email: String, clientId: String, token: String, confirmed: Boolean, expiration: Long, confirmExpiration: Long): Future[Unit] = ???

  /**
   * Find a user's login record by the user's email address.
   *
   * @param email The user's email address.
   * @return The login record if it exists.
   */
  def findLoginByEmail(email: String): Future[Option[Login]]

  /**
   * Find a user's login record by his login evidence (combination of assigned client id
   * sent to the client program and authentication token sent to the user via email).
   *
   * @param clientId The unique id sent to the client program.
   * @param token The authentication token sent to the user's email address.
   * @return The login record matching the login evidence if it exists.
   */
  def findLoginByAuthEvidence(clientId: String, token: String): Future[Option[Login]]

  /**
   * Find a user's login record by the assigned client id
   * sent to the client program.
   *
   * @param clientId The unique id sent to the client program.
   * @return The login record matching the client id if it exists.
   */
  def findLoginByClientId(clientId: String): Future[Option[Login]]

  /**
   * Remove a user's login record (logout the user).
   * @param email The user's email address.
   */
  def removeLogin(email: String): Future[Unit]

  /**
    * Use email as the unique identifier for updating the login record.
    * Must exist.
    */
  def updateLogin(email: String, confirmed: Boolean, expiration: Long): Future[Unit]

  /**
    * Remove the user with the given email and all traces of his identification.
    */
  def removeSignedUpUser(email: String): Future[Unit]
}