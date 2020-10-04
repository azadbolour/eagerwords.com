package controllers

import java.util.UUID

import org.scalatest.concurrent.ScalaFutures._
import com.bolour.app.kernel.common.domain.AuthEvidence
import KernelApiJsonSupport._
import com.bolour.app.kernel.common.message.{InitLoginRequest, InitLoginResponse}
import play.api.test.Helpers._

class AdminControllerSpec extends ControllerSpecBase {

  val myUserId = UUID.randomUUID().toString;
  val myUserEmail = s"${myUserId}@example.com"
  val nickname = myUserId

  val loginTimeout = 60; // Seconds.

  "admin controller should" {
    "remove signed up user" in {
      appService.reset()
      doSignUp(myUserEmail, nickname)
      // Successful login. The user is signed up.
      val authEvidence = doLogin(myUserEmail, nickname, loginTimeout)

      val future = adminController.unregisterUser()(mkRequest[AuthEvidence](authEvidence))
      val mustBeUnit = decodeJsonContent[Unit](future)
      mustBeUnit mustBe (())

      val initLoginRequest = InitLoginRequest(myUserEmail, loginTimeout)
      val resultFuture = baseController.initLogin()(mkRequest[InitLoginRequest](initLoginRequest))

      val sts = status(resultFuture)
      sts mustBe UNPROCESSABLE_ENTITY
      val json = contentAsJson(resultFuture)
      logger.info(s"not yet signed up: ${json}")
    }
    1
  }
}
