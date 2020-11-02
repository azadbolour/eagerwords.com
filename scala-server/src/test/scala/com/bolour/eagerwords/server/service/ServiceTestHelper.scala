package com.bolour.eagerwords.server.service

import org.scalatest.concurrent.ScalaFutures._
import org.scalatest.time.{Millis, Seconds, Span}

import com.bolour.util.StreamUtil.runAndMapStdOut
import com.bolour.util.email.EmailHelper.mockTokenExtractor
import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.auth.server.service.AuthService

object ServiceTestHelper {

  implicit val patienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

  def testingSignUp(service: AuthService)(email: String, nickname: String): AuthEvidence = {
    val (clientId, token) = runAndMapStdOut(mockTokenExtractor) {
      val clientId = service.initSignUp(email, nickname).futureValue
      clientId
    }
    service.confirmSignUp(clientId, token).futureValue
    val loginEvidence = AuthEvidence(clientId, token)
    loginEvidence
  }

  def testingLogin(service: AuthService)(email: String, nickname: String, loginTimeout: Long): AuthEvidence = {
    val (clientId, token) = runAndMapStdOut(mockTokenExtractor) {
      val clientId = service.initLogin(email, loginTimeout).futureValue
      clientId
    }
    service.confirmLogin(clientId, token)
    val loginEvidence = AuthEvidence(clientId, token)
    loginEvidence
  }
}
