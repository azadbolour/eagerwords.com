package controllers

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.app.kernel.server.domain.KernelExceptions.{BasicAppException, InternalAppException, MissingUserException, SystemOverloadedException}
import com.bolour.app.kernel.server.service.KernelService
import com.bolour.eagerwords.server.domain.GameExceptions.{GameException, InactiveGameException, InvalidCrosswordsException, InvalidWordException, MalformedPlayException, MissingGameException, MissingPieceException}
import com.bolour.eagerwords.server.service.{AdminService, GameService}
import com.bolour.language.domain.LanguageExceptions.{LanguageException, MissingDictionaryException, UnsupportedLanguageException}
import controllers.GameDtoConverters.{toInactiveGameErrorDto, toInvalidCrosswordsErrorDto, toInvalidWordErrorDto, toMalformedPlayErrorDto, toMissingDictionaryErrorDto, toMissingGameErrorDto, toMissingPieceErrorDto, toUnsupportedLanguageErrorDto}
import controllers.KernelApiJsonSupport._
import controllers.KernelDtoConverters.{toInternalErrorDto, toMissingUserErrorDto, toSystemOverloadedErrorDto}
import javax.inject.{Inject, Singleton}
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsError, JsResult, JsSuccess, JsValue, Json, Writes}
import play.api.mvc.{Action, ControllerComponents, Result}

import controllers.KernelApiJsonSupport._
import controllers.GameApiJsonSupport._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

@Singleton
class AdminController @Inject() (
  cc: ControllerComponents,
  authService: KernelService,
  service: AdminService) extends ControllerBase(cc) {

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

  // TODO. URGENT. Duplicated code (from GameController). Factor out.
  private def unprocessable(th: Throwable) = {

    def jsonError[DTO](dto: DTO)(implicit writes: Writes[DTO]) = UnprocessableEntity(Json.toJson(dto))

    val ex: Exception = th match {
      case appEx: BasicAppException => appEx
      case langEx: LanguageException => langEx
      case gameEx: GameException => gameEx
      case _ => InternalAppException("internal error", th)
    }

    // TODO. Add LanguageExceptions to this list.
    ex match {
      case ex: MissingPieceException => jsonError(toMissingPieceErrorDto(ex))
      case ex: MissingGameException => jsonError(toMissingGameErrorDto(ex))
      case ex: InactiveGameException => jsonError(toInactiveGameErrorDto(ex))
      case ex: MissingUserException => jsonError(toMissingUserErrorDto(ex))
      case ex: SystemOverloadedException => jsonError(toSystemOverloadedErrorDto(ex))
      case ex: InvalidWordException => jsonError(toInvalidWordErrorDto(ex))
      case ex: InvalidCrosswordsException => jsonError(toInvalidCrosswordsErrorDto(ex))
      case ex: UnsupportedLanguageException => jsonError(toUnsupportedLanguageErrorDto(ex))
      case ex: MissingDictionaryException => jsonError(toMissingDictionaryErrorDto(ex))
      case ex: MalformedPlayException => jsonError(toMalformedPlayErrorDto(ex))
      case ex: InternalAppException => jsonError(toInternalErrorDto(ex))
      case ex: Exception => jsonError(ex.getMessage)
    }
  }

}
