/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.auth.controller

import javax.inject._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import play.api.libs.json._
import play.api.mvc._
import org.slf4j.LoggerFactory

import com.bolour.util.controller.BaseDtoConverters.toInternalErrorDto
import com.bolour.util.controller.ControllerHelper._
import com.bolour.util.domain.BaseExceptions.{BaseException, InternalAppException}
import com.bolour.util.message.BaseApiJsonSupport._
import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.auth.common.message._
import com.bolour.auth.server.domain.AuthExceptions._
import com.bolour.auth.server.service.AuthService

import AuthApiJsonSupport._

// TODO. URGENT. Test login expiration disallows protected calls.

/**
  * Controller for authentication.
  */
@Singleton
class AuthController @Inject() (cc: ControllerComponents, service: AuthService) extends AbstractController(cc) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Action for initiating a sign-up sequence.
    */
  def initSignUp() = Action.async(parse.json) { implicit request =>
    logger.info(s"initSignUp json request: ${request.body}")
    val maybeValidInitSignUpRequest = validate[InitSignUpRequest](request)
    processRequest(maybeValidInitSignUpRequest, initSignUpValidated)
  }

  // TODO URGENT. Validate email.
  // TODO URGENT. Validate nickname - alphanumeric + ".-_" only.

  /**
    * Action for initiating sign-up sequence once sign-up request is validated.
    */
  def initSignUpValidated(request: InitSignUpRequest): Future[Result] = {
    logger.info(s"initSignUp request: ${request}")
    service.initSignUp(request.email, request.nickname).transformWith(triedClientId =>
      Future.successful(
        triedClientId match {
          case Failure(ex) =>
            logger.error("initSignUp failure", ex)
            (unprocessable(ex))
          case Success(clientId) =>
            logger.info("initSignUp success")
            val response = InitSignUpResponse(clientId)
            Ok(Json.toJson(response))
        }
      )
    )
  }

  /**
    * Action for confirming a sign-up.
    */
  def confirmSignUp() = Action.async(parse.json) { implicit request =>
    logger.info(s"confirmSignUp json request: ${request.body}")
    val maybeValidEvidence = validate[AuthEvidence](request)
    processRequest(maybeValidEvidence, confirmSignUpValidated)
  }

  /**
    * Action for confirming sign-up once the confirm request is validated.
    */
  def confirmSignUpValidated(request: AuthEvidence): Future[Result] = {
    logger.info(s"confirmSignUp request: ${request}")
    service.confirmSignUp(request.clientId, request.token).transformWith(triedUnit =>
      Future.successful(
        triedUnit match {
          case Failure(ex) =>
            logger.error("confirmSignUp failure", ex)
            (unprocessable(ex))
          case Success(_) =>
            logger.info("initSignUp success")
            Ok(Json.toJson(()))
        }
      )
    )
  }

  /**
    * Action for initiating a login sequence.
    */
  def initLogin() = Action.async(parse.json) { implicit request =>
    logger.info(s"initLogin json request: ${request.body}")
    val maybeValidInitLoginRequest = validate[InitLoginRequest](request)
    processRequest(maybeValidInitLoginRequest, initLoginValidated)
  }

  // TODO URGENT. Validate email.

  /**
    * Action for initiating sign-up sequence once sign-up request is validated.
    */
  def initLoginValidated(request: InitLoginRequest): Future[Result] = {
    logger.info(s"initLogin request: ${request}")
    service.initLogin(request.email).transformWith(triedClientId =>
      Future.successful(
        triedClientId match {
          case Failure(ex) =>
            logger.error("initLogin failure", ex)
            (unprocessable(ex))
          case Success(clientId) =>
            logger.info("initLogin success")
            val response = InitLoginResponse(clientId)
            Ok(Json.toJson(response))
        }
      )
    )
  }

  /**
    * Action for confirming a login.
    */
  def confirmLogin() = Action.async(parse.json) { implicit request =>
    logger.info(s"confirmLogin json request: ${request.body}")
    val maybeValidEvidence = validate[AuthEvidence](request)
    processRequest(maybeValidEvidence, confirmLoginValidated)
  }

  /**
    * Action for confirming login once the confirm request is validated.
    */
  def confirmLoginValidated(request: AuthEvidence): Future[Result] = {
    logger.info(s"confirmLogin request: ${request}")
    service.confirmLogin(request.clientId, request.token).transformWith(triedNickname =>
      Future.successful(
        triedNickname match {
          case Failure(ex) =>
            logger.error("confirmLogin failure", ex)
            (unprocessable(ex))
          case Success(nickname) =>
            logger.info("confirmLogin success")
            val response = ConfirmLoginResponse(nickname)
            Ok(Json.toJson(response))
        }
      )
    )
  }

  /**
    * Action for checking of a client is logged in.
    *
    * If the authentication evidence the action is unauthorized.
    * If it is found, then return true if the login has not expired.
    */
  def isLoggedIn() = Action.async(parse.json) { implicit request =>
    logger.info(s"isLoggedIn json request: ${request.body}")
    val maybeValidEvidence = validate[AuthEvidence](request)
    // TODO. URGENT. Validate that both clientId and token are non-null UUIDs. Copy everywhere.
    processRequest(maybeValidEvidence, isLoggedInValidated)
  }

  /**
    * Action for checking if a client is logged in once the evidence is validated.
    */
  def isLoggedInValidated(request: AuthEvidence): Future[Result] = {
    logger.info(s"isLoggedIn request: ${request}")
    service.isLoggedIn(request.clientId, request.token).transformWith(triedLoggedIn =>
      Future.successful(
        triedLoggedIn match {
          case Failure(ex) =>
            logger.error("isLoggedIn failure", ex)
            (unprocessable(ex))
          case Success(isLoggedIn) =>
            logger.info("isLoggedIn success")
            Ok(Json.toJson(isLoggedIn))
        }
      )
    )
  }

  def logout() = Action.async(parse.json) { implicit request =>
    logger.info(s"logout json request: ${request.body}")
    val maybeValidEvidence = validate[AuthEvidence](request)
    // TODO. URGENT. Validate that both clientId and token are non-null UUIDs. Copy everywhere.
    processRequest(maybeValidEvidence, logoutValidated)
  }

  def logoutValidated(request: AuthEvidence): Future[Result] = {
    logger.info(s"logout request: ${request}")
    service.logout(request.clientId, request.token).transformWith(triedLogout =>
      Future.successful(
        triedLogout match {
          case Failure(ex) =>
            logger.error("logout failure", ex)
            (unprocessable(ex))
          case Success(_) =>
            logger.info("logout success")
            Ok(Json.toJson(()))
        }
      )
    )
  }

  /**
    * Generic function to process a request using the result of validation of its payload.
    *
    * @param maybeValid Either a valid payload (DTO) or an error.
    * @param validProcessor Function to process a valid payload (DTO).
    * @tparam DTO Type of deserialized payload.
    *
    * @return Result including the response to be sent to the client.
    */
  private def processRequest[DTO](maybeValid: JsResult[DTO], validProcessor: DTO => Future[Result]): Future[Result] =
    maybeValid match {
      case JsError(errors) => Future.successful(badRequest(errors))
      case JsSuccess(v, _) => validProcessor(v)
    }

  /**
    * Create an unprocessable entity response from an exception.
    */
  private def unprocessable(th: Throwable): Result = {

    th match {
      case baseEx: BaseException => baseExceptionToUnprocessableEntity(baseEx)
      case authEx: AuthException => authExceptionToUnprocessableEntity(authEx)
      case other: Exception => unprocessableJson(toInternalErrorDto(InternalAppException(other.getMessage, other.getCause)))
    }

//    ex match {
//      case ex: MissingUserException => unprocessableJson(toMissingUserErrorDto(ex))
//      case ex: SystemOverloadedException => unprocessableJson(toSystemOverloadedErrorDto(ex))
//      case ex: InternalAppException => unprocessableJson(toInternalErrorDto(ex))
//      case ex: InvalidEmailException => unprocessableJson(toInvalidEmailErrorDto(ex))
//      case ex: InvalidNicknameException => unprocessableJson(toInvalidNicknameErrorDto(ex))
//      case ex: AlreadySignedUpException => unprocessableJson(toAlreadySignedUpErrorDto(ex))
//      case ex: AlreadyConfirmedException => unprocessableJson(toAlreadyConfirmedErrorDto(ex))
//      case ex: MissingAuthEvidenceException => unprocessableJson(toMissingAuthEvidenceErrorDto(ex))
//      case ex: SignUpConfirmationWithoutInitializationException =>
//        unprocessableJson(toSignUpConfirmationWithoutInitializationErrorDto(ex))
//      case ex: LoginConfirmationWithoutInitializationException =>
//        unprocessableJson(toLoginConfirmationWithoutInitializationErrorDto(ex))
//      case ex: ConfirmationTokenMismatchException => unprocessableJson(toConfirmationTokenMismatchErrorDto(ex))
//      case ex: ConfirmationTimeoutException => unprocessableJson(toConfirmationTimeoutErrorDto(ex))
//      case ex: NotSignedUpException => unprocessableJson(toNotSignedUpErrorDto(ex))
//      case ex: AccessViolationException => unprocessableJson(toAccessViolationErrorDto(ex))
//      case ex: MaxTimeoutExceededException => unprocessableJson(toMaxTimeoutExceededErrorDto(ex))
//      case ex: Exception => unprocessableJson(toInternalErrorDto(InternalAppException(ex.getMessage, ex.getCause)))
//    }
  }

}
