/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package controllers

import com.bolour.app.kernel.common.message.{
  AccessViolationErrorDto,
  AlreadyConfirmedErrorDto,
  AlreadySignedUpErrorDto,
  ConfirmationTimeoutErrorDto,
  ConfirmationTokenMismatchErrorDto,
  InternalErrorDto,
  InvalidEmailErrorDto,
  InvalidNicknameErrorDto,
  LoginConfirmationWithoutInitializationErrorDto,
  MaxTimeoutExceededErrorDto,
  MissingAuthEvidenceErrorDto,
  MissingUserErrorDto,
  NotSignedUpErrorDto,
  SignUpConfirmationWithoutInitializationErrorDto,
  SystemOverloadedErrorDto
}
import com.bolour.app.kernel.server.domain.KernelExceptions.{
  AccessViolationException,
  AlreadyConfirmedException,
  AlreadySignedUpException,
  ConfirmationTimeoutException,
  ConfirmationTokenMismatchException,
  InternalAppException,
  InvalidEmailException,
  InvalidNicknameException,
  LoginConfirmationWithoutInitializationException,
  MaxTimeoutExceededException,
  MissingAuthEvidenceException,
  MissingUserException,
  NotSignedUpException,
  SignUpConfirmationWithoutInitializationException,
  SystemOverloadedException
}

object KernelDtoConverters {

  // def fromUserDto(dto: UserDto): User = domain.User(UserObject.unAssignedId, dto.userId, dto.name, dto.email, dto.licenseAccepted)
  // def toUserDto(user: User): UserDto = UserDto(user.userId, user.name, user.email, user.licenseAccepted)

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

  /*
  def toAlreadyLoggedInErrorDto(ex: AlreadyLoggedInException) =
    AlreadyLoggedInErrorDto("AlreadyLoggedInError", ex.getMessage)
   */

  def toAlreadyConfirmedErrorDto(ex: AlreadyConfirmedException) =
    AlreadyConfirmedErrorDto("AlreadyConfirmedError", ex.getMessage)

  def toNotSignedUpErrorDto(ex: NotSignedUpException) =
    NotSignedUpErrorDto("NotSignedUpError", ex.getMessage, ex.email)

  def toAccessViolationErrorDto(ex: AccessViolationException) =
    AccessViolationErrorDto("AccessViolationError", ex.getMessage)

  def toMaxTimeoutExceededErrorDto(ex: MaxTimeoutExceededException) =
    MaxTimeoutExceededErrorDto("MaxTimeoutExceeded", ex.getMessage, ex.timeout, ex.maxTimeout)

  // toMaxTimeoutExceededErrorDto

  def toMissingUserErrorDto(ex: MissingUserException) =
    MissingUserErrorDto("MissingUserError", ex.getMessage, ex.userId)

  def toSystemOverloadedErrorDto(ex: SystemOverloadedException) =
    SystemOverloadedErrorDto("SystemOverloadedError", ex.getMessage)

  def toInternalErrorDto(ex: InternalAppException) =
    InternalErrorDto("InternalError", ex.getMessage)

}
