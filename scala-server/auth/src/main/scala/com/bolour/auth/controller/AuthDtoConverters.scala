package com.bolour.auth.controller

import com.bolour.auth.common.message._
import com.bolour.auth.server.domain.AuthExceptions._

object AuthDtoConverters {

  def toInvalidEmailErrorDto(ex: InvalidEmailException) =
    InvalidEmailErrorDto("InvalidEmailError", ex.getMessage, ex.email)

  def toInvalidNicknameErrorDto(ex: InvalidNicknameException) =
    InvalidNicknameErrorDto("InvalidNicknameError", ex.getMessage, ex.nickname)

  def toMissingAuthEvidenceErrorDto(ex: MissingAuthEvidenceException) =
    MissingAuthEvidenceErrorDto("MissingAuthEvidenceError", ex.getMessage)

  def toSignUpConfirmationWithoutInitializationErrorDto(ex: SignUpConfirmationWithoutInitializationException) =
    SignUpConfirmationWithoutInitializationErrorDto("SignUpConfirmationWithoutInitializationError", ex.getMessage)

  def toLoginConfirmationWithoutInitializationErrorDto(ex: LoginConfirmationWithoutInitializationException) =
    LoginConfirmationWithoutInitializationErrorDto("LoginConfirmationWithoutInitializationError", ex.getMessage)

  def toConfirmationTokenMismatchErrorDto(ex: ConfirmationTokenMismatchException) =
    ConfirmationTokenMismatchErrorDto("ConfirmationTokenMismatchError", ex.getMessage)

  def toConfirmationTimeoutErrorDto(ex: ConfirmationTimeoutException) =
    ConfirmationTimeoutErrorDto("ConfirmationTimeoutError", ex.getMessage)

  def toAlreadySignedUpErrorDto(ex: AlreadySignedUpException) =
    AlreadySignedUpErrorDto("AlreadySignedUpError", ex.getMessage)

  def toAlreadyConfirmedErrorDto(ex: AlreadyConfirmedException) =
    AlreadyConfirmedErrorDto("AlreadyConfirmedError", ex.getMessage)

  def toNotSignedUpErrorDto(ex: NotSignedUpException) =
    NotSignedUpErrorDto("NotSignedUpError", ex.getMessage, ex.email)

  def toAccessViolationErrorDto(ex: AccessViolationException) =
    AccessViolationErrorDto("AccessViolationError", ex.getMessage)

  def toMaxTimeoutExceededErrorDto(ex: MaxTimeoutExceededException) =
    MaxTimeoutExceededErrorDto("MaxTimeoutExceeded", ex.getMessage, ex.timeout, ex.maxTimeout)

  def toMissingUserErrorDto(ex: MissingUserException) =
    MissingUserErrorDto("MissingUserError", ex.getMessage, ex.userId)
}
