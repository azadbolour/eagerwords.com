/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.service

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.app.kernel.server.domain.{EmailUser, Login, User}
import com.bolour.util.CommonUtil.Email
import com.bolour.util.TimeUtil.nowSecs
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

/**
  * Trait representing the generic service layer of an application.
  */
trait KernelService {

  // TODO. Add parameter validation common to all implementations.

  protected val loginTimeout = 24 * 60 * 60 // For now make it a day. TODO. Use config option.
  protected val confirmationTimeout = 3 * 60;

  /**
    * Migrate the persistent data.
    */
  def migrate(): Future[Unit]

  /**
    * Clear ut the persistent data (for testing).
    */
  def reset(): Future[Unit]

  def findEmailUser(email: String): Future[Option[EmailUser]]
  def getEmailUser(email: String): Future[EmailUser]

  /**
    * Initialize the sign-up sequence. Send an authentication token
    * to the user's email so that he can confirm his ownership of the
    * email by a subsequent confirmSignUp.
    *
    * @param email The user's email.
    * @param nickname The user's nickname.
    *
    * @return A client id to be sent back to the user in response
    *         to the initiation of the sign-up process, to uniquely
    *         identify the client through which the user is requesting
    *         a sign-up.
    */
  def initSignUp(email: String, nickname: String): Future[String]

  /**
    * Confirmation by the user that he is indeed the owner of the purported
    * email. The authentication token sent to the user's email is provided
    * in this call as proof that the user owns the the email. For additional
    * security, the token is only accepted if it is accompanied by the clientId,
    * the identification of the client program that initiated the sign-up request.
    * The clientId is sent securely back to the client in the initial request.
    * The email cannot be guaranteed to be secure. But the token is useless
    * to an email spoofer without the client id.
    *
    * An error is signalled if the user is already signed in, if the timeout
    * between sign-in initiation and sign-in confirmation has expired, and if
    * the provided authentication evidence does not exist.
    *
    * A confirmed signup will also log the user in.
    *
    * @param clientId Unique id of the client program that initiated the
    *                 sign-up request (returned to the client in response to
    *                 sign-up initiation).
    * @param token The authentication token sent to the user's email
    *              to verify the user's ownership of the email.
    */
  def confirmSignUp(clientId: String, token: String): Future[Unit]

  /**
    * Initialize a login request. Similar to initSignUp.
    */
  def initLogin(email: String, timeout: Long = loginTimeout): Future[String]

  /**
    * Confirm a login request. Similar to confirmSignUp.
    * Returns the user's nickname.
    */
  def confirmLogin(clientId: String, token: String): Future[String]

  /**
    * Is the use identified by the combination of client id and authentication
    * token logged in.
    *
    * @param clientId The unique id of the client program used by the user.
    * @param token The authentication token send by email to the user to
    *              confirm his ownership of the email.
    */
  def isLoggedIn(clientId: String, token: String): Future[Boolean]

  def checkLogin(clientId: String, token: String): Future[Login]

  /**
    * Logout a user - given evidence of login: the clientId and the
    * email verification token.
    *
    * Note. For security reasons, this is the only manner in which the
    * user can log out. If the user loses his browser session of initial
    * login, the user will not be able to log out, but his login will,
    * of course, expire after its timeout.
    *
    * To continue work from another browser session, the use may
    * log in again - logging in again invalidates the existing login
    * for the given email and starts a new login session.
    *
    * @param clientId The unique id of the client used to log in.
    * @param token The token sent to the user's email to verify the user's
    *              ownership of the email.
    * @return
    */
  def logout(clientId: String, token: String): Future[Unit]

}

object KernelService {
  val serviceConfigPrefix = "service"
  def confPath(pathInService: String) =  s"${serviceConfigPrefix}.${pathInService}"
}
