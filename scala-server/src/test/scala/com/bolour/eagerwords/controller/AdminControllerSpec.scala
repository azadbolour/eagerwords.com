package com.bolour.eagerwords.controller

import java.util.UUID
import scala.concurrent.Await
import play.api.test.Helpers._

import com.bolour.util.message.BaseApiJsonSupport._
import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.auth.common.message.InitLoginRequest
import com.bolour.auth.controller.AuthApiJsonSupport._
import com.bolour.auth.controller.AuthControllerSpecBase
import com.bolour.eagerwords.server.service.{AdminServiceImpl, GameServiceImpl}

class AdminControllerSpec extends AuthControllerSpecBase {

  val myUserId = UUID.randomUUID().toString;
  val myUserEmail = s"${myUserId}@example.com"
  val nickname = myUserId

  val loginTimeout = 60; // Seconds.

  "admin controller should" {
    "remove signed up user" in {
      authService.reset()
      doSignUp(myUserEmail, nickname)
      // Successful login. The user is signed up.
      val authEvidence = doLogin(myUserEmail, nickname, loginTimeout)

      val gameService = new GameServiceImpl(config, None, authService)
      val adminService = new AdminServiceImpl(authService, gameService)
      val adminController = new AdminController(stubControllerComponents(), authService, adminService)

      Await.result(gameService.migrate(), timeout)
      Await.result(gameService.reset(), timeout)

      val future = adminController.unregisterUser()(mkRequest[AuthEvidence](authEvidence))
      val mustBeUnit = decodeJsonContent[Unit](future)
      mustBeUnit mustBe (())

      val initLoginRequest = InitLoginRequest(myUserEmail, loginTimeout)
      val resultFuture = authController.initLogin()(mkRequest[InitLoginRequest](initLoginRequest))

      val sts = status(resultFuture)
      sts mustBe UNPROCESSABLE_ENTITY
      val json = contentAsJson(resultFuture)
      logger.info(s"not yet signed up: ${json}")
    }
    1
  }
}
