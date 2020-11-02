/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.auth.controller

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import play.api.libs.json._
import play.api.mvc.{Result, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import org.scalatest.concurrent.ScalaFutures.{PatienceConfig, _}
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatestplus.play.PlaySpec
import org.slf4j.LoggerFactory

import com.bolour.util.StreamUtil.runAndMapStdOut
import com.bolour.util.email.EmailHelper.mockTokenExtractor
import com.bolour.util.message.BaseApiJsonSupport.unitReads
import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.auth.common.message._
import com.bolour.auth.controller.AuthApiJsonSupport._
import com.bolour.auth.server.service.{AuthServiceImpl, SecretServiceConfigImpl}

class AuthControllerSpecBase extends PlaySpec with Results {
  val logger = LoggerFactory.getLogger(this.getClass)

  val timeout = 5.seconds
  val config = ConfigFactory.load()

  // Needed for futureValue. Hides the default provided by scalatest.
  implicit val patienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

  val secretService = new SecretServiceConfigImpl(config)

  val authService = new AuthServiceImpl(config, secretService, None)
  val authController = new AuthController(stubControllerComponents(), authService)

  Await.result(authService.migrate(), timeout)

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

  def doSignUp(myUserEmail: String, nickname: String): AuthEvidence = {
    val initSignUpRequest = InitSignUpRequest(myUserEmail, nickname)
    val (clientId, token) = runAndMapStdOut(mockTokenExtractor) {
      val resultFuture = authController.initSignUp()(mkRequest[InitSignUpRequest](initSignUpRequest))
      val result = resultFuture.futureValue
      // TODO. URGENT. Avoid back-to-the-future decoding.
      val initSignUpResponse = decodeJsonContent[InitSignUpResponse](Future.successful(result))
      initSignUpResponse.clientId
    }
    logger.info(s"init sign-up clientId: $clientId, token: $token")

    val authEvidence = AuthEvidence(clientId, token)
    val resultFuture = authController.confirmSignUp()(mkRequest[AuthEvidence](authEvidence))
    decodeJsonContent[Unit](resultFuture)
    authEvidence
  }

  def doLogin(myUserEmail: String, nickname: String, loginTimeout: Long): AuthEvidence = {
    val initLoginRequest = InitLoginRequest(myUserEmail, loginTimeout)
    val (clientId, token) = runAndMapStdOut(mockTokenExtractor) {
      val resultFuture = authController.initLogin()(mkRequest[InitLoginRequest](initLoginRequest))
      val result = resultFuture.futureValue
      // TODO. URGENT. Avoid back-to-the-future decoding.
      val initLoginResponse = decodeJsonContent[InitLoginResponse](Future.successful(result))
      initLoginResponse.clientId
    }
    logger.info(s"init login clientId: $clientId, token: $token")

    val authEvidence = AuthEvidence(clientId, token)
    val resultFuture = authController.confirmLogin()(mkRequest[AuthEvidence](authEvidence))
    val response = decodeJsonContent[ConfirmLoginResponse](resultFuture)
    response.nickname mustBe nickname
    authEvidence
  }
}
