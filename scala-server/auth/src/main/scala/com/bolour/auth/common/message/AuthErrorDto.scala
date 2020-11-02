/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.auth.common.message

/**
  * Data transfer objects for authentication errors.
  */
sealed abstract class AuthErrorDto

case class InvalidEmailErrorDto(tag: String, message: String, email: String) extends AuthErrorDto

case class InvalidNicknameErrorDto(tag: String, message: String, nickname: String) extends AuthErrorDto

case class AlreadySignedUpErrorDto(tag: String, message: String) extends AuthErrorDto

case class AlreadyLoggedInErrorDto(tag: String, message: String) extends AuthErrorDto

case class AlreadyConfirmedErrorDto(tag: String, message: String) extends AuthErrorDto

case class MissingUserErrorDto(tag: String, message: String, userId: String) extends AuthErrorDto

case class MissingAuthEvidenceErrorDto(tag: String, message: String) extends AuthErrorDto

case class ConfirmationTimeoutErrorDto(tag: String, message: String) extends AuthErrorDto

case class SignUpConfirmationWithoutInitializationErrorDto(tag: String, message: String) extends AuthErrorDto

case class LoginConfirmationWithoutInitializationErrorDto(tag: String, message: String) extends AuthErrorDto

case class ConfirmationTokenMismatchErrorDto(tag: String, message: String) extends AuthErrorDto

case class NotSignedUpErrorDto(tag: String, message: String, email: String) extends AuthErrorDto

case class AccessViolationErrorDto(tag: String, message: String) extends AuthErrorDto

case class MaxTimeoutExceededErrorDto(tag: String, message: String, timeout: Long, maxTimeout: Long)

