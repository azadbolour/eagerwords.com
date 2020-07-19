/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.app.kernel.common.message

/**
  * Data transfer objects for basic application errors.
  */
sealed abstract class BasicAppErrorDto

case class InvalidEmailErrorDto(tag: String, message: String, email: String) extends BasicAppErrorDto

case class InvalidNicknameErrorDto(tag: String, message: String, nickname: String) extends BasicAppErrorDto

case class AlreadySignedUpErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class AlreadyLoggedInErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class AlreadyConfirmedErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class MissingUserErrorDto(tag: String, message: String, userId: String) extends BasicAppErrorDto

case class MissingAuthEvidenceErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class ConfirmationTimeoutErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class SignUpConfirmationWithoutInitializationErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class LoginConfirmationWithoutInitializationErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class ConfirmationTokenMismatchErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class NotSignedUpErrorDto(tag: String, message: String, email: String) extends BasicAppErrorDto

case class AccessViolationErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class MaxTimeoutExceededErrorDto(tag: String, message: String, timeout: Long, maxTimeout: Long)

case class SystemOverloadedErrorDto(tag: String, message: String) extends BasicAppErrorDto

case class InternalErrorDto(tag: String, message: String) extends BasicAppErrorDto
