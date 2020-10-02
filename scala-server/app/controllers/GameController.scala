/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package controllers

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.app.kernel.server.domain.{EmailConstants, Login}
import javax.inject._
import controllers.KernelApiJsonSupport._
import controllers.GameApiJsonSupport._
import controllers.GameDtoConverters._
import com.bolour.eagerwords.common.message.{StartGameRequest, _}
import com.bolour.eagerwords.server.domain.GameExceptions._
import com.bolour.eagerwords.server.service.GameService
import com.bolour.app.kernel.server.domain.KernelExceptions.{BasicAppException, InternalAppException, MissingUserException, SystemOverloadedException}
import com.bolour.app.kernel.server.service.KernelService
import com.bolour.language.domain.LanguageExceptions.{LanguageException, MissingDictionaryException, UnsupportedLanguageException}
import org.slf4j.LoggerFactory
import play.api.mvc.{Result, _}

import scala.util.{Failure, Success, Try}
import play.api.libs.json._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.KernelDtoConverters._

// TODO. URGENT. DO NOT log clientId and token from requests. Once the code is debugged.

/*
 * Calls made by guests would ideally have a clientId, but no token.
 * For this application that is overkill.
 *
 * But it can be used as an extra measure of security in other applications,
 * so random guest users would not be able to manipulate other guest users' data
 * by guessing the identifiers of that data.
 */

/*
 * Calls that deal with protected data require login evidence in their payloads.
 * Login evidence consists of a clientId and an authentication token.
 * The controller checks that the use is [still] logged in, and if
 * so obtains the email of of the user (it is stored in each login record).
 * The email is used as the user's identification.
 *
 * To accommodate calls made by guests, the login evidence is an
 * optional parameter in which None implies a guest user.
 */

/**
  * Controller for the eager words game API.
  *
  * Handlers for API requests that include a body are implemented in two steps.
  * The handler first validates the request payload, and then calls an internal
  * function to process the request. Conceptually the latter function should be
  * private. But it is made public to simplify tests for valid payloads.
  */
@Singleton
class GameController @Inject() (cc: ControllerComponents, service: GameService, authService: KernelService) extends ControllerBase(cc) {

  private val logger = LoggerFactory.getLogger(this.getClass)

  /**
   * Action for saving a user's game settings taking json input.
   */
  def saveUserGameSettings(): Action[JsValue] = Action.async(parse.json) { implicit request =>
    logger.info(s"saveUserGameSettings json request: ${request.body}")
    val maybeValidSaveUserGameSettingsRequest = validate[SaveUserGameSettingsRequest](request)
    processUserRequest(maybeValidSaveUserGameSettingsRequest, saveUserGameSettingsValidated)
  }

  /**
    * Action for saving a user once json input is validated.
    */
  def saveUserGameSettingsValidated(request: SaveUserGameSettingsRequest): Future[Result] = {
    val evidence = request.loginEvidence
    val settings = request.settings
    for {
      login <- authService.checkLogin(evidence.clientId, evidence.token)
      result <- service.saveUserGameSettings(login.email, settings).transformWith(triedUnit =>
        Future.successful(
          triedUnit match {
            case Failure (ex) =>
              logger.error ("saveUserUserGameSettings failure", ex)
              unprocessable (ex)
            case Success (_) =>
              logger.info ("saveUserUserGameSettings success")
              Ok (Json.toJson (()))
          }
        )
      )
    } yield (result)
  }

  /**
    * Action for getting the user's game settings.
    */
  def getUserGameSettings(): Action[JsValue] = Action.async(parse.json) { implicit request =>
    logger.info(s"getUserGameSettings json request: ${request.body}")
    val maybeValidGetUserGameSettingsRequest = validate[AuthEvidence](request)
    processUserRequest(maybeValidGetUserGameSettingsRequest, getUserGameSettingsValidated)
  }

  def getUserGameSettingsValidated(evidence: AuthEvidence): Future[Result] = {
    for {
      login <- authService.checkLogin(evidence.clientId, evidence.token)
      result <- service.getUserGameSettings(login.email).transformWith(triedGet => Future.successful(
        triedGet match {
          case Failure(ex) =>
            logger.info("getUserGameSettings failure", ex)
            unprocessable(ex)
          case Success(settings) =>
            logger.info(s"getUserGameSettings success")
            Ok(Json.toJson(settings))
        }
      ))
    } yield(result)
  }

  /**
   * Action for starting a game taking json input.
   */
  def startGame: Action[JsValue] = Action.async(parse.json) { implicit request =>
    logger.info(s"startGame json request: ${request.body}")
    val maybeValidStartGameRequest = validate[StartGameRequest](request)
    processRequest(maybeValidStartGameRequest, startGameValidated)
  }

  /**
    * Action for starting a game once json input is validated.
    */
  def startGameValidated(startGameRequest: StartGameRequest): Future[Result] = {
    logger.info(s"startGame startGameRequest: ${startGameRequest}")
    val StartGameRequest(maybeLoginEvidence, gameParams, initPieces) = startGameRequest
    for {
      maybeLogin: Option[Login] <- maybeLoginEvidence match {
            case None => Future.successful(None)
            case Some(evidence) => authService.checkLogin(evidence.clientId, evidence.token).map(l => Some(l))
          }
      maybeEmail = maybeLogin.map(_.email)
      result <- service.startGame(gameParams, initPieces, maybeEmail).transformWith(triedGame => Future.successful(
        triedGame match {
          case Failure(ex) =>
            logger.error("startGame failure", ex)
            unprocessable(ex)
          case Success(game) =>
            val gameDto = mkStartGameResponse(game)
            logger.info(s"startGame success gameDto: ${gameDto}")
            Ok(Json.toJson(gameDto))
        }
      ))
    } yield (result)
  }

  // TODO. Change all logging to debug.

  /**
    * Action for committing a play taking json input.
    */
  def commitPlay(gameId: String): Action[JsValue] = Action.async(parse.json) { implicit request =>
    logger.info(s"commitPlay: json request: ${request.body}")
    val maybeValidPlayPieces = validate[CommitPlayRequest](request)
    processGameRequest(gameId, maybeValidPlayPieces, commitPlayValidated)
  }

  /**
    * Authorize access to game data by checking game ownership, and return the user's email
    * address. Guests have a fixed dummy email.
    *
    * @param gameId Unique id of the game.
    * @param loginEvidence Optional user's login evidence - None meaning guest user. Some for
    *                      registered users.
    * @return Future of the email address or exception if registered user is logged out.
    *         Guests have a fixed dummy email address.
    */
  private def authorizeGameAccess(gameId: String, loginEvidence: Option[AuthEvidence]): Future[String] = {
    for {
      email <- loginEvidence match {
        case None => Future.successful(EmailConstants.guestEmail)
        case Some (evidence) =>
          authService.checkLogin(evidence.clientId, evidence.token).map(_.email)
      }
      _ = logger.info(s"authorizeGameAccess - email: ${email}")
      _ <- service.checkGameOwner(gameId, email)
    } yield email
  }

  /**
    * Action for committing a play once the play has been validated.
    */
  def commitPlayValidated(gameId: String)(request: CommitPlayRequest): Future[Result] = {
    logger.info(s"commitPlay play request: ${request}")
    val evidence = request.loginEvidence
    val playPieces = request.playPieces
    for {
      _ <- authorizeGameAccess(gameId, evidence)
      result <- service.commitPlay(gameId, playPieces).transformWith(triedCommit => Future.successful(
        triedCommit match {
          case Failure (ex) =>
            logger.error ("commitPlay failure", ex)
            unprocessable (ex)
          case Success ((miniState, replacementPieces, deadPoints)) =>
            logger.info (s"commitPlay success - replacements: ${replacementPieces}, mini state: ${miniState}")
            val response = CommitPlayResponse (miniState, replacementPieces, deadPoints)
            Ok (Json.toJson (response))
        }
      ))
    } yield result
  }

  def machinePlay(gameId: String): Action[JsValue] = Action.async(parse.json) { implicit request =>
    logger.info(s"machinePlay")
    val maybeValidLoginEvidence = validate[Option[AuthEvidence]](request)
    processGameRequest(gameId, maybeValidLoginEvidence, machinePlayValidated)
  }

  def machinePlayValidated(gameId: String)(evidence: Option[AuthEvidence]): Future[Result] = {
    for {
      _ <- authorizeGameAccess(gameId, evidence)
      result <- service.machinePlay(gameId).transformWith(triedMachinePlay => Future.successful(
        triedMachinePlay match {
          case Failure(ex) =>
            logger.info("machinePlay failure", ex)
            unprocessable(ex)
          case Success((miniState, playedPieces, deadPoints)) =>
            logger.info(s"machinePlay success - playedPieces: ${playedPieces}, mini state: ${miniState}")
            val response = MachinePlayResponse(miniState, playedPieces, deadPoints)
            Ok(Json.toJson(response))
        }
      ))
    } yield(result)
  }

  /**
    * Action for swapping a piece.
    */
  def swapPiece(gameId: String): Action[JsValue] = Action.async(parse.json) { implicit request =>
    logger.info(s"swapPiece: json request: ${request.body}")
    val maybeValidRequest = validate[SwapPieceRequest](request)
    processGameRequest(gameId, maybeValidRequest, swapPieceValidated)
  }

  /**
    * Action for swapping a piece once the piece has been validated.
    */
  def swapPieceValidated(gameId: String)(request: SwapPieceRequest): Future[Result] = {
    logger.info(s"swapPiece piece: ${request}")
    val evidence = request.loginEvidence
    val piece = request.piece

    for {
      _ <- authorizeGameAccess(gameId, evidence)
      result <- service.swapPiece(gameId, piece).transformWith(triedSwap => Future.successful(
        triedSwap match {
          case Failure (ex) =>
            logger.error ("swapPiece failure", ex)
            unprocessable (ex)
          case Success ((miniState, newPiece)) =>
            logger.info (s"swapPiece success - new piece: ${newPiece}, mini state: ${miniState}")
            val response = SwapPieceResponse (miniState, newPiece)
            Ok (Json.toJson (response))
        }
      ))
    } yield result
  }

  private def getFullGameAction(gameId: String): Future[GetFullGameResponse] = {
    logger.info(s"getting full game - gameId: ${gameId}")
    for {
      maybeGame <- service.findGameById(gameId)
      result <- maybeGame match {
        case None =>
          logger.info(s"getFullGameAction: missing game: ${gameId}")
          Future.failed(MissingGameException(gameId))
        case Some(game) =>
          val fullGameResponse = mkGetFullGameResponse(game)
          logger.info(s"getFullGameAction: full game response: ${fullGameResponse}")
          Future.successful(fullGameResponse)
      }
    } yield result
  }

  def getFullGame(gameId: String): Action[JsValue] = {
    logger.info(s"getFullGame - gameId: ${gameId}")
    gameAuthWrapper(gameId, "getFullGame") {
      getFullGameAction(gameId)
    }
  }

  def closeGame(gameId: String): Action[JsValue] =
    gameAuthWrapper(gameId, "close") {
      service.endGame(gameId)
    }

  def suspendGame(gameId: String): Action[JsValue] =
    gameAuthWrapper(gameId, "suspend") {
      service.suspendGame(gameId)
    }

  def cancelGame(gameId: String): Action[JsValue] =
    gameAuthWrapper(gameId, "cancel") {
      service.cancelGame(gameId)
    }

  def resignGame(gameId: String): Action[JsValue] =
    gameAuthWrapper(gameId, "resign") {
      service.resignGame(gameId)
    }

  def resumeGame(gameId: String): Action[JsValue] =
    gameAuthWrapper(gameId, "resume") {
      service.resumeGame(gameId).map(_ => ())
    }

  /**
    * Action for getting the a user's unfinished games.
    */
  def getUnfinishedUserGames(): Action[JsValue] = {
    val action: String => Future[GetGamesResponse] = (email) =>
      service.getUnfinishedUserGames(email).map(mkGetUserGamesResponse)
    userAuthWrapper(action)
  }

  /**
    * Action for getting a list of a given user's games.
    */
  def getUserGames(): Action[JsValue] = Action.async(parse.json) { implicit request =>
    logger.info(s"getUserGames json request: ${request.body}")
    val maybeValidGetUserGamesRequest = validate[GetUserGamesRequest](request)
    processUserRequest(maybeValidGetUserGamesRequest, getUserGamesValidated)
  }

  /**
    * Action for getting a list of a given user's games once json input is validated.
    */
  def getUserGamesValidated(getUserGamesRequest: GetUserGamesRequest): Future[Result] = {
    logger.info(s"getUserGames getUserGamesRequest: ${getUserGamesRequest}")
    val GetUserGamesRequest(loginEvidence, fromEpochSecond, toEpochSecond, maxGames) = getUserGamesRequest

    for {
      login <- authService.checkLogin(loginEvidence.clientId, loginEvidence.token)
      result <- service.getUserGames(login.email, fromEpochSecond, toEpochSecond, maxGames).
        transformWith(tried => tryToFutureResult(tried.map(mkGetUserGamesResponse)))
    } yield result
  }

  /**
    * Generic abstraction for controller methods that have a gameId path parameter,
    * and whose payload is just a login evidence to prove authentication.
    * Checks that the user is logged in, and that the logged in user owns the
    * game identified by the gameId, and then executes the core action of the method.
    *
    * @param gameId The gameId path parameter value.
    * @param action A code block that returns implements the core action of this controller method
    *               and returns a Future of the required response value.
    * @tparam Response The type of the response value. It must have a Writes instance
    *                  so it can be written as json in the payload of the HTTP response.
    */
  private def gameAuthWrapper[Response: Writes](gameId: String, actionName: String)(action: => Future[Response]): Action[JsValue] =
    Action.async(parse.json) { implicit request =>
      val maybeValidEvidence = validate[Option[AuthEvidence]](request)
      logger.info(s"gameAuthWrapper - maybeValidEvidence: ${maybeValidEvidence}")
      maybeValidEvidence match {
        case JsError(errors) => Future.successful(badRequest(errors))
        case JsSuccess(evidence, _) =>
          logger.info(s"gameAuthWrapper - success - evidence: ${evidence}")
          for {
            _ <- authorizeGameAccess(gameId, evidence).transformWith(tried => tryToFutureResult(tried, actionName))
            _ = logger.info(s"gameAuthWrapper - authorized")
            result <- action.transformWith(tried => tryToFutureResult(tried, actionName))
          } yield result
      }
  }

  /**
    * Generic abstraction for controller methods that have no path parameters,
    * and whose payload is just a login evidence proving authentication.
    * Checks that the user is logged in and then executes the core action of the
    * controller method giving it the logged-in user's email.
    *
    * @param action The core action of the controller method as a function of the user's email.
    * @tparam Response The type of the response value. It must have a Writes instance
    *                  so it can be written as json in the payload of the HTTP response.
    */
  private def userAuthWrapper[Response: Writes](action: String => Future[Response]): Action[JsValue] =
    Action.async(parse.json) { implicit request =>
      val maybeValidEvidence = validate[AuthEvidence](request)
      maybeValidEvidence match {
        case JsError(errors) => Future.successful(badRequest(errors))
        case JsSuccess(evidence, _) =>
          for {
            login <- authService.checkLogin(evidence.clientId, evidence.token)
            result <- action(login.email).transformWith(tried => tryToFutureResult(tried))
          } yield result
      }
  }

  // Then refactor all controller calls to use this method with action name to reduce boilerplate.

  /**
    * Convert a Try to a Future with appropriate logging.
    *
    * @param tried The Try to be converted.
    * @param actionName The display name of the action for logging.
    * @tparam T The generic type of the Try.
    * @return A completed Future corresponding to the try.
    */
  private def tryToFutureResult[T: Writes](tried: Try[T], actionName: String = "action"): Future[Result] = {
    Future.successful(
      tried match {
        case Failure(ex) =>
          logger.error(s"${actionName} failed", ex)
          unprocessable(ex)
        case Success(response) =>
          logger.info(s"${actionName} succeeded")
          Ok(Json.toJson(response))
      }
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

  // maybeValid.fold(badRequest, validProcessor)
  // Future.fromTry(JsResult.toTry(maybeValid))

  /**
    * Generic function to process a request pertaining to a particular game.
    *
    * @see #processRequest
    */
  private def processGameRequest[DTO](gameId: String, maybeValid: JsResult[DTO],
    validProcessor: String => DTO => Future[Result]): Future[Result] =
    maybeValid match {
      case JsError(errors) => Future.successful(badRequest(errors))
      case JsSuccess(v, _) => validProcessor(gameId)(v)
    }

  // maybeValid.fold(badRequest, validProcessor(gameId))

  // TODO. Move to ControllerBase. Generic for all controllers.
  /**
    * Generic function to process a request pertaining to a particular user.
    *
    * @see #processRequest
    */
  private def processUserRequest[DTO](maybeValid: JsResult[DTO],
    validProcessor: DTO => Future[Result]): Future[Result] =
    maybeValid match {
      case JsError(errors) => Future.successful(badRequest(errors))
      case JsSuccess(v, _) => validProcessor(v)
    }

  // maybeValid.fold(badRequest, validProcessor(userId))

  // TODO. Include kernel exceptions here.
  // TODO. Make unprocessable method of kernel exceptions public.
  // TODO. And move all unprocessable functions out of controller modules.
  /**
    * Create an unprocessable entity response from an exception.
    */
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

//  def closeGame(gameId: String): Action[AnyContent] = Action.async { implicit request =>
//    logger.info(s"closeGame")
//    service.endGame(gameId).transformWith(triedSummary => Future.successful(
//      triedSummary match {
//        case Failure(ex) =>
//          logger.error("closeGame failure", ex)
//          unprocessable(ex)
//        case Success(summary) =>
//          logger.info("closeGame success")
//          Ok(Json.toJson(summary))
//      }
//    ))
//  }

/**
  * Action for suspending a game.
  */
//  def suspendGame(gameId: String): Action[AnyContent] = Action.async { implicit request =>
//    logger.info(s"suspendGame")
//    service.suspendGame(gameId).transformWith(triedSuspend => Future.successful(
//      triedSuspend match {
//        case Failure(ex) =>
//          logger.error("suspendGame failure", ex)
//          unprocessable(ex)
//        case Success(_) =>
//          logger.info("suspendGame success")
//          Ok(Json.toJson(()))
//      }
//    ))
//  }

/**
  * Action for resuming a game.
  */
//  def resumeGame(gameId: String): Action[AnyContent] = Action.async { implicit request =>
//    logger.info(s"resumeGame")
//    service.resumeGame(gameId).transformWith(triedGame => Future.successful(
//      triedGame match {
//        case Failure(ex) =>
//          logger.error("resumeGame failure", ex)
//          unprocessable(ex)
//        case Success(game) =>
//          val gameDto = mkResumeGameResponse(game)
//          logger.info(s"resumeGame success gameDto: ${gameDto}")
//          Ok(Json.toJson(gameDto))
//      }
//    ))
//  }

/**
  * Action for cancelling a game.
  */
//  def cancelGame(gameId: String): Action[AnyContent] = Action.async { implicit request =>
//    logger.info(s"cancelGame")
//    service.cancelGame(gameId).transformWith(tried => Future.successful(
//      tried match {
//        case Failure(ex) =>
//          logger.error("cancelGame failure", ex)
//          unprocessable(ex)
//        case Success(_) =>
//          logger.info("cancelGame success")
//          Ok(Json.toJson(()))
//      }
//    ))
//  }

/**
  * Action for user resigning.
  */
//  def resignGame(gameId: String): Action[AnyContent] = Action.async { implicit request =>
//    logger.info(s"resignGame")
//    service.resignGame(gameId).transformWith(tried => Future.successful(
//      tried match {
//        case Failure(ex) =>
//          logger.error("cancelGame failure", ex)
//          unprocessable(ex)
//        case Success(_) =>
//          logger.info("cancelGame success")
//          Ok(Json.toJson(()))
//      }
//    ))
//  }

//  def getUnfinishedUserGames(userId: String): Action[AnyContent] = Action.async { implicit request =>
//    logger.info(s"getUnfinishedUserGames - userId: $userId")
//    service.getUnfinishedUserGames(userId).transformWith(tried => Future.successful(
//      tried match {
//        case Failure(ex) =>
//          logger.error("getUnfinishedUserGames failure", ex)
//          unprocessable(ex)
//        case Success(games) =>
//          val getGamesResponse = mkGetUserGamesResponse(games)
//          logger.info(s"getUnfinishedUserGames success - response: ${getGamesResponse}")
//          Ok(Json.toJson(getGamesResponse))
//      }
//    ))
//  }

//    getUserGamesRequest match {
//      case GetUserGamesRequest(fromEpochSecond, toEpochSecond, maxGames) => {
//        // TODO. Validate parameters. non-negative.
//        service.getUserGames(userId, fromEpochSecond, toEpochSecond, maxGames).transformWith(triedListGames => Future.successful(
//          triedListGames match {
//            case Failure(ex) =>
//              logger.error("getUserGames failure", ex)
//              unprocessable(ex)
//            case Success(games) => {
//              val getGamesResponse = mkGetUserGamesResponse(games)
//              logger.info(s"getUserGames success - response: ${getGamesResponse}")
//              Ok(Json.toJson(getGamesResponse))
//            }
//          }
//        ))
//      }
//    }
//  }
