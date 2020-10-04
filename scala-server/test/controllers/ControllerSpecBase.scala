/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package controllers

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.app.kernel.common.message.{ConfirmLoginResponse, InitLoginRequest, InitLoginResponse, InitSignUpRequest, InitSignUpResponse}
import com.bolour.app.kernel.server.domain
import com.bolour.app.kernel.server.domain.User
import com.bolour.app.kernel.server.service.{KernelServiceImpl, SecretServiceConfigImpl}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import org.scalatestplus.play.PlaySpec
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json, Reads, Writes}
import play.api.mvc.{Result, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import com.bolour.util.CommonUtil.Email
import com.bolour.app.util.server.KernelUtil.stringId
import com.bolour.eagerwords.common.domain.DeviceType.MouseDevice
import com.bolour.eagerwords.common.domain.{GameParams, GamePlayParams, PieceProviderType, PlayerType, UserGameSettings}
import com.bolour.eagerwords.server.service.{AdminServiceImpl, GameServiceImpl}
import com.bolour.util.StreamUtil.runAndMapStdOut
import controllers.GameApiJsonSupport._
import org.scalatest.time.{Millis, Seconds, Span}
import org.slf4j.LoggerFactory
import controllers.KernelApiJsonSupport.unitReads
import org.scalatest.concurrent.ScalaFutures.PatienceConfig
import org.scalatest.concurrent.ScalaFutures._
import controllers.KernelApiJsonSupport._
import com.bolour.util.email.EmailHelper.mockTokenExtractor
import com.bolour.util.mail.AbstractSmtpMailService.EMAIL_CONTENT_LABEL

class ControllerSpecBase extends PlaySpec with Results {
  val logger = LoggerFactory.getLogger(this.getClass)

  val timeout = 5.seconds
  val config = ConfigFactory.load()

  // Needed for futureValue. Hides the default provided by scalatest.
  implicit val patienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

  val secretService = new SecretServiceConfigImpl(config)

  // TODO. URGENT. Make sure the appService uses the mock email service for tests.
  val basicAppService = new KernelServiceImpl(config, secretService, None)

  val appService = new KernelServiceImpl(config, secretService, None)
  val gameService = new GameServiceImpl(config, None, appService)
  val adminService = new AdminServiceImpl(appService, gameService)
  val baseController = new KernelController(stubControllerComponents(), basicAppService)
  val gameController = new GameController(stubControllerComponents(), gameService, appService)
  val adminController = new AdminController(stubControllerComponents(), appService, adminService)

  val name = "Bill"
  val email: Email = Email("bill@example.com")
  val userId = "12345"

  val dimension = 5
  val squarePixels = 20
  val trayCapacity = 5
  val languageCode = "tiny"
  val startingPlayer = PlayerType.UserPlayer
  val genType = PieceProviderType.Cyclic

//  val settings = GameSettings(dimension, squarePixels, trayCapacity, languageCode,
//    PieceProviderType.Cyclic, Some(startingPlayer), Some(MouseDevice))
  val pointValues = List.fill(dimension, dimension)(1)

  val playSettings: GamePlayParams = GamePlayParams(dimension, trayCapacity, languageCode, genType, Some(startingPlayer))
  val gameParams = GameParams(playSettings, pointValues)

  // val gameParams = GameParams(settings, pointValues)
  val center = dimension/2

  val player = domain.User(stringId, userId, name, email, true)
  Await.result(appService.migrate(), timeout)
  Await.result(gameService.migrate(), timeout)
  Await.result(gameService.reset(), timeout)
  // Await.result(service.saveUser(player), timeout)

  var result: Future[Result] = null
  // For debugging: logger.info(s"${contentAsString(result)}")

  def mkRequest[DTO](dto: DTO)(implicit writes: Writes[DTO]): FakeRequest[JsValue] =
    FakeRequest().withBody(Json.toJson[DTO](dto))

  def decodeJsonContent[DTO](result: Future[Result])(implicit reads: Reads[DTO]): DTO = {
    // TODO. Check for status OK - can also be BadRequest or Unprocessable.
    val bodyJson = contentAsJson(result)
    val jsResult = Json.fromJson[DTO](bodyJson)
    jsResult match {
      case JsSuccess(value, _) => value
      case JsError(errors) => throw new NoSuchElementException(errors.toString)
    }
  }

//  def createUser(): Unit = {
//    val playerDto = new UserDto(userId, name, email, true)
//    result = baseController.saveUser()(mkRequest[UserDto](playerDto))
//    decodeJsonContent[Unit](result)
//  }


  def doSignUp(myUserEmail: String, nickname: String): AuthEvidence = {
    val initSignUpRequest = InitSignUpRequest(myUserEmail, nickname)
    val (clientId, token) = runAndMapStdOut(mockTokenExtractor) {
      val resultFuture = baseController.initSignUp()(mkRequest[InitSignUpRequest](initSignUpRequest))
      val result = resultFuture.futureValue
      // TODO. URGENT. Avoid back-to-the-future decoding.
      val initSignUpResponse = decodeJsonContent[InitSignUpResponse](Future.successful(result))
      initSignUpResponse.clientId
    }
    logger.info(s"init sign-up clientId: $clientId, token: $token")

    val authEvidence = AuthEvidence(clientId, token)
    val resultFuture = baseController.confirmSignUp()(mkRequest[AuthEvidence](authEvidence))
    decodeJsonContent[Unit](resultFuture)
    authEvidence
  }

  def doLogin(myUserEmail: String, nickname: String, loginTimeout: Long): AuthEvidence = {
    val initLoginRequest = InitLoginRequest(myUserEmail, loginTimeout)
    val (clientId, token) = runAndMapStdOut(mockTokenExtractor) {
      val resultFuture = baseController.initLogin()(mkRequest[InitLoginRequest](initLoginRequest))
      val result = resultFuture.futureValue
      // TODO. URGENT. Avoid back-to-the-future decoding.
      val initLoginResponse = decodeJsonContent[InitLoginResponse](Future.successful(result))
      initLoginResponse.clientId
    }
    logger.info(s"init login clientId: $clientId, token: $token")

    val authEvidence = AuthEvidence(clientId, token)
    val resultFuture = baseController.confirmLogin()(mkRequest[AuthEvidence](authEvidence))
    val response = decodeJsonContent[ConfirmLoginResponse](resultFuture)
    response.nickname mustBe nickname
    authEvidence
  }

}
