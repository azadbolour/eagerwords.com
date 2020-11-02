package com.bolour.auth.controller

import play.api.libs.json.{OWrites, Reads}
import play.api.libs.json.Json.{reads, writes}
import play.api.mvc.Result

import com.bolour.util.controller.ControllerHelper.unprocessableJson
import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.auth.common.message._
import com.bolour.auth.controller.AuthDtoConverters._
import com.bolour.auth.server.domain.AuthExceptions._

/**
  * Implicit JSON readers and writers for basic app dto objects.
  */
object AuthApiJsonSupport {

  implicit val initSignUpRequestReads: Reads[InitSignUpRequest] = reads[InitSignUpRequest]
  implicit val initSignUpResponseReads: Reads[InitSignUpResponse] = reads[InitSignUpResponse]
  implicit val initLoginRequestReads: Reads[InitLoginRequest] = reads[InitLoginRequest]
  implicit val initLoginResponseReads: Reads[InitLoginResponse] = reads[InitLoginResponse]
  implicit val confirmLoginResponseReads: Reads[ConfirmLoginResponse] = reads[ConfirmLoginResponse]
  implicit val authEvidenceReads: Reads[AuthEvidence] = reads[AuthEvidence]

  implicit val initSignUpRequestWrites: OWrites[InitSignUpRequest] = writes[InitSignUpRequest]
  implicit val initSignUpResponseWrites: OWrites[InitSignUpResponse] = writes[InitSignUpResponse]
  implicit val initLoginRequestWrites: OWrites[InitLoginRequest] = writes[InitLoginRequest]
  implicit val initLoginResponseWrites: OWrites[InitLoginResponse] = writes[InitLoginResponse]
  implicit val confirmLoginResponseWrites: OWrites[ConfirmLoginResponse] = writes[ConfirmLoginResponse]
  implicit val authEvidenceWrites: OWrites[AuthEvidence] = writes[AuthEvidence]

  implicit val invalidEmailErrorDtoWrites: OWrites[InvalidEmailErrorDto] = writes[InvalidEmailErrorDto]
  implicit val invalidNicknameErrorDtoWrites: OWrites[InvalidNicknameErrorDto] = writes[InvalidNicknameErrorDto]
  implicit val missingAuthEvidenceErrorDtoWrites: OWrites[MissingAuthEvidenceErrorDto] = writes[MissingAuthEvidenceErrorDto]
  implicit val signUpConfirmationWithoutInitializationErrorDtoWrites: OWrites[SignUpConfirmationWithoutInitializationErrorDto] =
    writes[SignUpConfirmationWithoutInitializationErrorDto]
  implicit val loginConfirmationWithoutInitializationErrorDtoWrites: OWrites[LoginConfirmationWithoutInitializationErrorDto] =
    writes[LoginConfirmationWithoutInitializationErrorDto]
  implicit val confirmationTokenMismatchErrorDtoWrites: OWrites[ConfirmationTokenMismatchErrorDto] =
    writes[ConfirmationTokenMismatchErrorDto]
  implicit val confirmationTimeoutErrorDtoWrites: OWrites[ConfirmationTimeoutErrorDto] = writes[ConfirmationTimeoutErrorDto]
  implicit val alreadySignedUpDtoWrites: OWrites[AlreadySignedUpErrorDto] = writes[AlreadySignedUpErrorDto]
  implicit val alreadyLoggedInDtoWrites: OWrites[AlreadyLoggedInErrorDto] = writes[AlreadyLoggedInErrorDto]
  implicit val alreadyConfirmedDtoWrites: OWrites[AlreadyConfirmedErrorDto] = writes[AlreadyConfirmedErrorDto]
  implicit val notSignedUpDtoWrites: OWrites[NotSignedUpErrorDto] = writes[NotSignedUpErrorDto]
  implicit val accessViolationDtoWrites: OWrites[AccessViolationErrorDto] = writes[AccessViolationErrorDto]
  implicit val maxTimeoutExceededDtoWrites: OWrites[MaxTimeoutExceededErrorDto] = writes[MaxTimeoutExceededErrorDto]
  implicit val missingUserErrorDtoWrites: OWrites[MissingUserErrorDto] = writes[MissingUserErrorDto]

  def authExceptionToUnprocessableEntity(ex: AuthException): Result = {
    ex match {
      case ex: MissingUserException => unprocessableJson(toMissingUserErrorDto(ex))
      case ex: InvalidEmailException => unprocessableJson(toInvalidEmailErrorDto(ex))
      case ex: InvalidNicknameException => unprocessableJson(toInvalidNicknameErrorDto(ex))
      case ex: AlreadySignedUpException => unprocessableJson(toAlreadySignedUpErrorDto(ex))
      case ex: AlreadyConfirmedException => unprocessableJson(toAlreadyConfirmedErrorDto(ex))
      case ex: MissingAuthEvidenceException => unprocessableJson(toMissingAuthEvidenceErrorDto(ex))
      case ex: SignUpConfirmationWithoutInitializationException =>
        unprocessableJson(toSignUpConfirmationWithoutInitializationErrorDto(ex))
      case ex: LoginConfirmationWithoutInitializationException =>
        unprocessableJson(toLoginConfirmationWithoutInitializationErrorDto(ex))
      case ex: ConfirmationTokenMismatchException => unprocessableJson(toConfirmationTokenMismatchErrorDto(ex))
      case ex: ConfirmationTimeoutException => unprocessableJson(toConfirmationTimeoutErrorDto(ex))
      case ex: NotSignedUpException => unprocessableJson(toNotSignedUpErrorDto(ex))
      case ex: AccessViolationException => unprocessableJson(toAccessViolationErrorDto(ex))
      case ex: MaxTimeoutExceededException => unprocessableJson(toMaxTimeoutExceededErrorDto(ex))
    }
  }
}
