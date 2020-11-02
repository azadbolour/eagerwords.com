package com.bolour.eagerwords.controller

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import play.api.libs.json._
import play.api.mvc.{AbstractController, Action, ControllerComponents, Result}
import org.slf4j.LoggerFactory

import com.bolour.util.controller.BaseDtoConverters.toInternalErrorDto
import com.bolour.util.controller.ControllerHelper._
import com.bolour.util.domain.BaseExceptions.{BaseException, InternalAppException}
import com.bolour.util.message.BaseApiJsonSupport._
import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.auth.server.domain.AuthExceptions.AuthException
import com.bolour.auth.server.service.AuthService
import com.bolour.language.domain.LanguageExceptions.LanguageException
import com.bolour.eagerwords.common.message.GameApiJsonSupport._
import com.bolour.eagerwords.server.domain.GameExceptions.GameException
import com.bolour.eagerwords.server.service.AdminService

import com.bolour.auth.controller.AuthApiJsonSupport._

@Singleton
class AdminController @Inject() (
  cc: ControllerComponents,
  authService: AuthService,
  service: AdminService) extends AbstractController(cc) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  def unregisterUser(): Action[JsValue] = Action.async(parse.json) { implicit request =>
    logger.info(s"unregisterUser json request: ${request.body}")
    val validated = validate[AuthEvidence](request)
    processRequest(validated, unregisterUserValidated)
  }

  def unregisterUserValidated(evidence: AuthEvidence): Future[Result] = {
    for {
      login <- authService.checkLogin(evidence.clientId, evidence.token)
      result <- service.removeSignedUpUser(login.email).transformWith(triedUnit =>
        Future.successful(
          triedUnit match {
            case Failure (ex) =>
              logger.error ("unregisterUser failure", ex)
              unprocessable (ex)
            case Success (_) =>
              logger.info ("unregisterUser success")
              Ok (Json.toJson (()))
          }
        )
      )
    } yield (result)
  }

  // TODO. URGENT. Duplicated code. Factor out to ControllerBase.
  private def processRequest[DTO](maybeValid: JsResult[DTO], validProcessor: DTO => Future[Result]): Future[Result] =
    maybeValid match {
      case JsError(errors) => Future.successful(badRequest(errors))
      case JsSuccess(v, _) => validProcessor(v)
    }

  private def unprocessable(th: Throwable) = {

    th match {
      case baseEx: BaseException => baseExceptionToUnprocessableEntity(baseEx)
      case authEx: AuthException => authExceptionToUnprocessableEntity(authEx)
      case langEx: LanguageException => languageExceptionToUnprocessableEntity(langEx)
      case gameEx: GameException => gameExceptionToUnprocessableEntity(gameEx)
      case other: Exception => unprocessableJson(toInternalErrorDto(InternalAppException(other.getMessage, other.getCause)))
    }


    //    // def jsonError[DTO](dto: DTO)(implicit writes: Writes[DTO]) = UnprocessableEntity(Json.toJson(dto))
//
//    val ex: Exception = th match {
//      case appEx: AuthException => appEx
//      case langEx: LanguageException => langEx
//      case gameEx: GameException => gameEx
//      case _ => InternalAppException("internal error", th)
//    }
//
//    // TODO. Add LanguageExceptions to this list.
//    ex match {
//      case ex: MissingPieceException => unprocessableJson(toMissingPieceErrorDto(ex))
//      case ex: MissingGameException => unprocessableJson(toMissingGameErrorDto(ex))
//      case ex: InactiveGameException => unprocessableJson(toInactiveGameErrorDto(ex))
//      case ex: MissingUserException => unprocessableJson(toMissingUserErrorDto(ex))
//      case ex: SystemOverloadedException => unprocessableJson(toSystemOverloadedErrorDto(ex))
//      case ex: InvalidWordException => unprocessableJson(toInvalidWordErrorDto(ex))
//      case ex: InvalidCrosswordsException => unprocessableJson(toInvalidCrosswordsErrorDto(ex))
//      case ex: UnsupportedLanguageException => unprocessableJson(toUnsupportedLanguageErrorDto(ex))
//      case ex: MissingDictionaryException => unprocessableJson(toMissingDictionaryErrorDto(ex))
//      case ex: MalformedPlayException => unprocessableJson(toMalformedPlayErrorDto(ex))
//      case ex: InternalAppException => unprocessableJson(toInternalErrorDto(ex))
//      case ex: Exception => unprocessableJson(ex.getMessage)
//    }
  }

}
