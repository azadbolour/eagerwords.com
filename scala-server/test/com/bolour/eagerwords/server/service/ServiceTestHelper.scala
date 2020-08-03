package com.bolour.eagerwords.server.service

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.app.kernel.server.service.KernelService
import com.bolour.util.StreamUtil.runAndMapStdOut
import com.bolour.util.email.EmailHelper.mockTokenExtractor
import org.scalatest.concurrent.ScalaFutures._
import org.scalatest.time.{Millis, Seconds, Span}

object ServiceTestHelper {

  implicit val patienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

  def testingSignUp(service: KernelService)(email: String, nickname: String): AuthEvidence = {
    val (clientId, token) = runAndMapStdOut(mockTokenExtractor) {
      val clientId = service.initSignUp(email, nickname).futureValue
      clientId
    }
    service.confirmSignUp(clientId, token).futureValue
    val loginEvidence = AuthEvidence(clientId, token)
    loginEvidence
  }

  def testingLogin(service: KernelService)(email: String, nickname: String, loginTimeout: Long): AuthEvidence = {
    val (clientId, token) = runAndMapStdOut(mockTokenExtractor) {
      val clientId = service.initLogin(email, loginTimeout).futureValue
      clientId
    }
    service.confirmLogin(clientId, token)
    val loginEvidence = AuthEvidence(clientId, token)
    loginEvidence
  }
}
