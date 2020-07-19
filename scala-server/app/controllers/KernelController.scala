/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package controllers

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.app.kernel.common.message.{ConfirmLoginResponse, HandShakeResponse, InitLoginRequest, InitLoginResponse, InitSignUpRequest, InitSignUpResponse}
import com.bolour.app.kernel.server.domain.KernelExceptions.{AccessViolationException, AlreadyConfirmedException, AlreadySignedUpException, BasicAppException, ConfirmationTimeoutException, ConfirmationTokenMismatchException, InternalAppException, InvalidEmailException, InvalidNicknameException, LoginConfirmationWithoutInitializationException, MaxTimeoutExceededException, MissingAuthEvidenceException, MissingUserException, NotSignedUpException, SignUpConfirmationWithoutInitializationException, SystemOverloadedException}
import com.bolour.app.kernel.server.service.KernelService
import com.bolour.app.util.server.KernelUtil.stringId
import controllers.KernelApiJsonSupport._
import controllers.KernelDtoConverters._
import javax.inject._
import org.slf4j.LoggerFactory
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

// TODO. URGENT. Test login expiration disallows protected calls.

/**
  * Controller for generic application API.
  *
  * Note. This controller properly belongs to the root module.
  * But I ran out of time troubleshooting getting it to work from there according
  * to these instructions:
  *   https://www.playframework.com/documentation/2.5.x/SBTSubProjects#Splitting-the-route-file
  * To be figured out when time permits.
  */
@Singleton
class KernelController @Inject() (cc: ControllerComponents, service: KernelService) extends ControllerBase(cc) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  val serverType = "Scala"
  val apiVersion = "1.0"
  val handShakeResponse = HandShakeResponse(serverType, apiVersion)

  def handShake: Action[AnyContent] = Action {
    Ok(Json.toJson(handShakeResponse))
  }

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
  private def unprocessable(th: Throwable) = {

    def jsonError[DTO](dto: DTO)(implicit writes: Writes[DTO]) = UnprocessableEntity(Json.toJson(dto))

    val ex: BasicAppException = th match {
      case appEx: BasicAppException => appEx
      case _ => InternalAppException("internal error", th)
    }

    ex match {
      case ex: MissingUserException => jsonError(toMissingUserErrorDto(ex))
      case ex: SystemOverloadedException => jsonError(toSystemOverloadedErrorDto(ex))
      case ex: InternalAppException => jsonError(toInternalErrorDto(ex))
      case ex: InvalidEmailException => jsonError(toInvalidEmailErrorDto(ex))
      case ex: InvalidNicknameException => jsonError(toInvalidNicknameErrorDto(ex))
      case ex: AlreadySignedUpException => jsonError(toAlreadySignedUpErrorDto(ex))
      case ex: AlreadyConfirmedException => jsonError(toAlreadyConfirmedErrorDto(ex))
      case ex: MissingAuthEvidenceException => jsonError(toMissingAuthEvidenceErrorDto(ex))
      case ex: SignUpConfirmationWithoutInitializationException =>
        jsonError(toSignUpConfirmationWithoutInitializationErrorDto(ex))
      case ex: LoginConfirmationWithoutInitializationException =>
        jsonError(toLoginConfirmationWithoutInitializationErrorDto(ex))
      case ex: ConfirmationTokenMismatchException => jsonError(toConfirmationTokenMismatchErrorDto(ex))
      case ex: ConfirmationTimeoutException => jsonError(toConfirmationTimeoutErrorDto(ex))
      case ex: NotSignedUpException => jsonError(toNotSignedUpErrorDto(ex))
      case ex: AccessViolationException => jsonError(toAccessViolationErrorDto(ex))
      case ex: MaxTimeoutExceededException => jsonError(toMaxTimeoutExceededErrorDto(ex))
    }
  }

}
