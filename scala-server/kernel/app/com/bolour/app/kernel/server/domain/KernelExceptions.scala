/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.domain

object KernelExceptions {

  sealed abstract class BasicAppException(cause: Throwable = null) extends Exception(cause) {
    def causeMessage: String = if (cause == null) "" else s" - caused by: ${cause.getMessage}"
  }

  case class InvalidEmailException(email: String) extends BasicAppException {
    override def getMessage = s"invalid email: ${email}"
  }

  case class InvalidNicknameException(nickname: String) extends BasicAppException {
    override def getMessage = s"invalid nickname [only alphanumeric and the special characters '._-' allowed]: ${nickname}"
  }

  case class MissingAuthEvidenceException() extends BasicAppException {
    override def getMessage = s"authentication evidence not found"
  }

  case class LoginConfirmationWithoutInitializationException() extends BasicAppException {
    override def getMessage = s"attempt to confirm a login process that has not been started"
  }

  case class SignUpConfirmationWithoutInitializationException() extends BasicAppException {
    override def getMessage = s"missing record of email for sign-up - sign-up must be initialized before it can be confirmed"
  }

  case class ConfirmationTokenMismatchException() extends BasicAppException {
    override def getMessage = s"confirmation token mismatch"
  }

  case class ConfirmationTimeoutException() extends BasicAppException {
    override def getMessage = s"confirmation timeout expired"
  }

  case class AlreadySignedUpException() extends BasicAppException {
    override def getMessage = s"already signed up"
  }

  /*
  case class AlreadyLoggedInException() extends BasicAppException {
    override def getMessage = s"already logged in"
  }

   */

  case class AlreadyConfirmedException() extends BasicAppException {
    override def getMessage = s"already confirmed"
  }

  case class MissingUserException(userId: String) extends BasicAppException {
    override def getMessage = s"user does not exist for: ${userId}"
  }

  case class NotSignedUpException(email: String) extends BasicAppException {
    override def getMessage = s"email: ${email} not yet signed up"
  }

  case class MaxTimeoutExceededException(timeout: Long, maxTimeout: Long) extends BasicAppException {
    override def getMessage = s"timeout: ${timeout} exceeds maximum of ${maxTimeout} seconds"
  }
  case class SystemOverloadedException() extends BasicAppException {
    override def getMessage: String = "system overloaded"
  }

  case class InternalAppException(message: String, cause: Throwable) extends BasicAppException(cause) {
    override def getMessage: String = s"${message}${causeMessage}"
  }

  case class AccessViolationException(objectType: String, objectDescriptor: String, violation: String)
    extends BasicAppException {
    override def getMessage: String = s"access to ${objectType}:${objectDescriptor} not allowed: ${violation}"
  }

}
