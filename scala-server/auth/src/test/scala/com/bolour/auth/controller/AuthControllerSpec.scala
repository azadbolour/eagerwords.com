package com.bolour.auth.controller

import java.util.UUID

class AuthControllerSpec extends AuthControllerSpecBase {

  val myUserId = UUID.randomUUID().toString;
  val myUserEmail = s"${myUserId}@example.com"
  val nickname = myUserId

  val loginTimeout = 60; // Seconds.

  // TODO. URGENT. Make sure the appService uses the mock email service.
  // TODO. URGENT. Controller tests for passwordless authentication - different scenarios.
  // See PasswordLessServiceSpec.

  // TODO. Move reset to a "before" method.

  "kernel controller should" {
    "sign up a new user" in {
      authService.reset()
      doSignUp(myUserEmail, nickname)
      doLogin(myUserEmail, nickname, loginTimeout)
    }
    1
  }

//  private def doSignUp = {
//    val initSignUpRequest = InitSignUpRequest(myUserEmail, nickname)
//    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
//      val resultFuture = baseController.initSignUp()(mkRequest[InitSignUpRequest](initSignUpRequest))
//      val result = resultFuture.futureValue
//      // TODO. URGENT. Avoid back-to-the-future decoding.
//      val initSignUpResponse = decodeJsonContent[InitSignUpResponse](Future.successful(result))
//      initSignUpResponse.clientId
//    }
//    logger.info(s"init sign-up clientId: $clientId, token: $token")
//
//    val authEvidence = AuthEvidence(clientId, token)
//    val resultFuture = baseController.confirmSignUp()(mkRequest[AuthEvidence](authEvidence))
//    val unit = decodeJsonContent[Unit](resultFuture)
//  }

//  private def doLogin = {
//    val initLoginRequest = InitLoginRequest(myUserEmail, loginTimeout)
//    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
//      val resultFuture = baseController.initLogin()(mkRequest[InitLoginRequest](initLoginRequest))
//      val result = resultFuture.futureValue
//      // TODO. URGENT. Avoid back-to-the-future decoding.
//      val initLoginResponse = decodeJsonContent[InitLoginResponse](Future.successful(result))
//      initLoginResponse.clientId
//    }
//    logger.info(s"init login clientId: $clientId, token: $token")
//
//    val authEvidence = AuthEvidence(clientId, token)
//    val resultFuture = baseController.confirmLogin()(mkRequest[AuthEvidence](authEvidence))
//    val response = decodeJsonContent[ConfirmLoginResponse](resultFuture)
//    response.nickname mustBe nickname
//  }

  //  "email should" {
  //    "be encoded and decoded correctly" in {
  //      val email: Email = Email("xyz@xyz.com")
  //      val json = Json.toJson(email)
  //      logger.info(s"email to json: $json")
  //      val decoded: Email = Json.fromJson[Email](json).get
  //      logger.info(s"decoded email: $decoded")
  //    }
  //    1
  //  }

  //  "kernel controller should" {
  //    "save and get user" in {
  //      val userDto = UserDto(myUserId, name, email, true)
  //      result = baseController.saveUser()(mkRequest[UserDto](userDto))
  //      val unit: Unit = decodeJsonContent[Unit](result)
  //      val userResult = baseController.getUser(myUserId)(FakeRequest())
  //      val gotUserDto: UserDto = decodeJsonContent[UserDto](userResult)
  //      gotUserDto mustEqual userDto
  //
  //      // Non-existent user is an Option of None and should come out as null.
  //      val noUserJson: JsValue = contentAsJson(baseController.getUser("non-existent")(FakeRequest()))
  //      logger.info(s"no user json: $noUserJson")
  //      noUserJson mustEqual JsNull
  //    }
  //    1 // TODO. Why is an Int needed here?
  //  }

}
