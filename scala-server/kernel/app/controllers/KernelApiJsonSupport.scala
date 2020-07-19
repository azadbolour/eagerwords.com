/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package controllers

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.app.kernel.common.message.{AccessViolationErrorDto, AlreadyConfirmedErrorDto, AlreadyLoggedInErrorDto, AlreadySignedUpErrorDto, ConfirmLoginResponse, ConfirmationTimeoutErrorDto, ConfirmationTokenMismatchErrorDto, HandShakeResponse, InitLoginRequest, InitLoginResponse, InitSignUpRequest, InitSignUpResponse, InternalErrorDto, InvalidEmailErrorDto, InvalidNicknameErrorDto, LoginConfirmationWithoutInitializationErrorDto, MaxTimeoutExceededErrorDto, MissingAuthEvidenceErrorDto, MissingUserErrorDto, NotSignedUpErrorDto, SignUpConfirmationWithoutInitializationErrorDto, SystemOverloadedErrorDto}
import play.api.libs.json._
import play.api.libs.json.Json.{reads, writes}
import com.bolour.util.CommonUtil.Email

/**
  * Implicit JSON readers and writers for basic app dto objects.
  */
object KernelApiJsonSupport {
  implicit def optionFormat[T: Format]: Format[Option[T]] = new Format[Option[T]]{
    override def reads(json: JsValue): JsResult[Option[T]] = json.validateOpt[T]

    override def writes(o: Option[T]): JsValue = o match {
      case Some(t) ⇒ implicitly[Writes[T]].writes(t)
      case None ⇒ JsNull
    }
  }

  implicit val charReads: Reads[Char] = new Reads[Char] {
    def reads(json: JsValue): JsResult[Char] = Json.fromJson[String](json) map { _.head }
  }

  implicit val emailReads: Reads[Email] = new Reads[Email] {
    def reads(json: JsValue): JsResult[Email] = {
      val string = Json.fromJson[String](json)
      string map (value => Email(value))
    }
  }

  /**
    * The json encoding of unit.
    *
    * Treat unit as an empty tuple which is encoded as an array of its field values.
    * The [String] tye parameter is provided to keep the compiler happy
    * (since String is implicitly convertible to json).
    */
  implicit val unitReads: Reads[Unit] = new Reads[Unit] {
    def reads(json: JsValue): JsSuccess[Unit] = {
      val result = Json.fromJson[List[String]](json)
      result.get match {
        case Nil => JsSuccess(())
        case _ => throw new IllegalArgumentException(s"unitRead: ${json.toString}")
      }
    }
  }



  // implicit val userDtoReads: Reads[UserDto] = reads[UserDto]
  implicit val handShakeResponseReads: Reads[HandShakeResponse] = reads[HandShakeResponse]

  implicit val initSignUpRequestReads: Reads[InitSignUpRequest] = reads[InitSignUpRequest]
  implicit val initSignUpResponseReads: Reads[InitSignUpResponse] = reads[InitSignUpResponse]
  implicit val initLoginRequestReads: Reads[InitLoginRequest] = reads[InitLoginRequest]
  implicit val initLoginResponseReads: Reads[InitLoginResponse] = reads[InitLoginResponse]
  implicit val confirmLoginResponseReads: Reads[ConfirmLoginResponse] = reads[ConfirmLoginResponse]
  implicit val authEvidenceReads: Reads[AuthEvidence] = reads[AuthEvidence]

  implicit val charWrites: Writes[Char] = new Writes[Char] {
    def writes(o: Char): JsValue = Json.toJson[String](o.toString)
  }

  implicit val emailWrites: Writes[Email] = new Writes[Email] {
    def writes(o: Email): JsValue = Json.toJson[String](o.email)
  }

  implicit val unitWrites: Writes[Unit] = new Writes[Unit] {
    def writes(o: Unit): JsValue = Json.toJson(List[String]())
  }

  // implicit val userDtoWrites: OWrites[UserDto] = writes[UserDto]
  implicit val initSignUpRequestWrites: OWrites[InitSignUpRequest] = writes[InitSignUpRequest]
  implicit val initSignUpResponseWrites: OWrites[InitSignUpResponse] = writes[InitSignUpResponse]
  implicit val initLoginRequestWrites: OWrites[InitLoginRequest] = writes[InitLoginRequest]
  implicit val initLoginResponseWrites: OWrites[InitLoginResponse] = writes[InitLoginResponse]
  implicit val confirmLoginResponseWrites: OWrites[ConfirmLoginResponse] = writes[ConfirmLoginResponse]
  implicit val authEvidenceWrites: OWrites[AuthEvidence] = writes[AuthEvidence]
  implicit val handShakeResponseWrites: OWrites[HandShakeResponse] = writes[HandShakeResponse]

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
  implicit val systemOverloadedErrorDtoWrites: OWrites[SystemOverloadedErrorDto] = writes[SystemOverloadedErrorDto]
  implicit val internalErrorDtoWrites: OWrites[InternalErrorDto] = writes[InternalErrorDto]

}
