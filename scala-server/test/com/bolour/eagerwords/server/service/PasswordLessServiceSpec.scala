package com.bolour.eagerwords.server.service

import java.io.{ByteArrayOutputStream, PrintStream}

import com.bolour.util.StreamUtil.runAndMapStdOut
import com.bolour.app.kernel.server.service.{KernelService, KernelServiceImpl, SecretServiceConfigImpl}
import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.ScalaFutures.PatienceConfig
import org.scalatest.concurrent.ScalaFutures._
import org.scalatest.OptionValues._
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import org.slf4j.LoggerFactory

class PasswordLessServiceSpec extends FlatSpec with Matchers with BeforeAndAfter {

  private val logger = LoggerFactory.getLogger(this.getClass)

  // Needed for futureValue. Hides the default provided by scalatest.
  implicit val patienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(10, Millis))

  // TODO. Configuration should include mock email service.
  val conf = ConfigFactory.load()
  val secretService = new SecretServiceConfigImpl(conf)
  val appService: KernelService = new KernelServiceImpl(conf, secretService, None)
  appService.migrate().futureValue

  before {
    appService.reset().futureValue
  }

  val userEmail = "nobody@faketestsite105.com"
  val nickname = "Bob"

  val emailContentLabel = "content: "

  def tokenExtractor(emailString: String): String = emailString.lines.find(
    line => line.startsWith(emailContentLabel)).get.drop(emailContentLabel.length
  )

  "passwordless service" should "sign-up a user" in {
    doSignUp
  }

  "passwordless service" should "login a user" in {
    doSignUp
    doLogin
  }

  "passwordless service" should "reject login attempt without having signed up" in {
    assertThrows[Exception] {
      doLogin
    }
  }

  "passwordless service" should "reject login attempt after partial sign-up" in {
    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
      appService.initSignUp(userEmail, nickname).futureValue
    }
    assertThrows[Exception] {
      doLogin
    }
  }

  "passwordless service" should "reject login attempt with a guessed token" in {
    doSignUp
    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
      appService.initLogin(userEmail).futureValue
    }
    assertThrows[Exception] {
      appService.confirmLogin(clientId, s"guessed${token}").futureValue
    }
  }

  "passwordless service" should "reject login attempt with a guessed clientId" in {
    doSignUp
    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
      appService.initLogin(userEmail).futureValue
    }
    assertThrows[Exception] {
      appService.confirmLogin(s"guessed${clientId}", token).futureValue
    }
  }

  "passwordless service" should "allow a sign-up sequence after an abandoned sign-up initiation" in {
    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
      appService.initSignUp(userEmail, nickname).futureValue
    }
    doSignUp
    // Continuing with the original sign-up should error out.
    assertThrows[Exception] {
      appService.confirmSignUp(clientId, token).futureValue;
    }
  }

  "passwordless service" should "allow a login sequence after an abandoned login initiation" in {
    doSignUp
    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
      appService.initLogin(userEmail).futureValue
    }
    doLogin
    // Continuing the original login should error out.
    assertThrows[Exception] {
      appService.confirmSignUp(clientId, token).futureValue;
    }
  }

  "passwordless service" should "prevent redundant sign-ups" in {
    doSignUp
    assertThrows[Exception] {
      val (clientId, token) = runAndMapStdOut(tokenExtractor) {
        appService.initSignUp(userEmail, nickname).futureValue
      }
    }
  }

  "passwordless service" should "allow a second login while already logged in" in {
    doSignUp
    doLogin
    doLogin
  }

  "passwordless service" should "reject sign-up confirmation without a corresponding initiation" in {
    assertThrows[Exception] {
      appService.confirmSignUp("12345678", "12345678").futureValue;
    }
  }

  "passwordless service" should "reject login confirmation without a corresponding initiation" in {
    assertThrows[Exception] {
      appService.confirmLogin("12345678", "12345678").futureValue;
    }
  }

  "passwordless service" should "reject redundant login confirmation" in {
    doSignUp
    val (clientId, token) = doLogin
    assertThrows[Exception] {
      appService.confirmLogin(clientId, token).futureValue;
    }
    val isLoggedIn = appService.isLoggedIn(clientId, token).futureValue
    isLoggedIn shouldBe true
  }

  private def doLogin = {
    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
      appService.initLogin(userEmail).futureValue
    }
    appService.confirmLogin(clientId, token).futureValue
    val isLoggedIn = appService.isLoggedIn(clientId, token).futureValue
    isLoggedIn shouldBe true
    (clientId, token)
  }

  private def doSignUp = {
    /*
     * Sign-up initialization sends a validation email to the user.
     * The email's content includes the authentication token.
     * Tests use the mock email service which prints the email fields
     * to stdout, one per line, and the token extractor retrieves the token
     * from the email.
     */
    val (clientId, token) = runAndMapStdOut(tokenExtractor) {
      appService.initSignUp(userEmail, nickname).futureValue
    }

    // Validate the sign-up by proving you got the token and hence you own the email.
    appService.confirmSignUp(clientId, token).futureValue;

    // A successful sign-up sequence creates a new email user.
    val emailUser = appService.findEmailUser(userEmail).futureValue.get
    emailUser.email shouldBe userEmail
    emailUser.nickname shouldBe nickname
    (clientId, token)
  }
}

// TODO. URGENT. Test login expiration.


