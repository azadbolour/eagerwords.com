/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package controllers

import java.util.UUID

import com.bolour.app.kernel.common.domain.AuthEvidence
import org.slf4j.LoggerFactory
import play.api.test._
import com.bolour.app.util.server.KernelUtil.stringId
import com.bolour.plane.domain.Point
import com.bolour.eagerwords.common.message._
import com.bolour.eagerwords.common.domain._
import com.bolour.eagerwords.server.domain.GameExceptions.InactiveGameException
import controllers.GameApiJsonSupport._
import controllers.KernelApiJsonSupport._

class GameControllerSpec extends ControllerSpecBase {

  override val logger = LoggerFactory.getLogger(this.getClass)

  val myUserId = UUID.randomUUID().toString;
  val myUserEmail = s"${myUserId}@example.com"
  val nickname = myUserId
  val loginTimeout = 60; // Seconds.

  // createUser()

  val signInEvidence: AuthEvidence = doSignUp(myUserEmail, nickname)

  "game controller" should {
    var theGameId: String = null
    var theUserTrayPieces: List[Piece] = Nil

    val uPieces = List(Piece('B', stringId()), Piece('E', stringId()), Piece('T', stringId())) // User to play "BET".
    val mPieces = List(Piece('S', stringId()), Piece('T', stringId()), Piece('Z', stringId())) // Machine to play "SET" using user's 'E'.
    val initPieces = InitPieces(Nil, uPieces, mPieces)

    def theTest(isGuest: Boolean) = {

      val loginEvidence: Option[AuthEvidence] =
        if(isGuest) None
        else Some(doLogin(myUserEmail, nickname, loginTimeout))

      val startGameRequest = StartGameRequest(loginEvidence, gameParams, initPieces)

      result = gameController.startGame()(mkRequest(startGameRequest))
      val startGameResponse = decodeJsonContent[StartGameResponse](result)
      logger.info(s"startGameResponse: ${startGameResponse}")
      startGameResponse match {
        case StartGameResponse(gameId, piecePoints, userTrayPieces) =>
          piecePoints.size mustEqual 0
          userTrayPieces.size mustEqual gameParams.trayCapacity
          theGameId = gameId
          theUserTrayPieces = userTrayPieces
      }

      val userPlayPieces = List(
        PlayPiece(uPieces(0), Point(center, center - 1), true),
        PlayPiece(uPieces(1), Point(center, center), true),
        PlayPiece(uPieces(2), Point(center, center + 1), true)
      )

      val commitPlayRequest = CommitPlayRequest(loginEvidence, userPlayPieces)

      result = gameController.commitPlay(theGameId)(mkRequest(commitPlayRequest))
      decodeJsonContent[CommitPlayResponse](result) match {
        case CommitPlayResponse(score, replacementPieces, deadPoints) =>
          replacementPieces.size mustEqual 3
      }

      // TODO. How to make request with no body the PlaySpec way??
      // result = gameController.machinePlay(theGameId)(FakeRequest())
      result = gameController.machinePlay(theGameId)(mkRequest(loginEvidence))
      decodeJsonContent[MachinePlayResponse](result) match {
        case MachinePlayResponse(score, playedPieces, deadPoints) =>
          playedPieces.size must be > 2
      }

      val swappedPiece = theUserTrayPieces(0)
      val swapRequest = SwapPieceRequest(loginEvidence, swappedPiece)
      result = gameController.swapPiece(theGameId)(mkRequest(swapRequest))
      val SwapPieceResponse(_, newPiece) = decodeJsonContent[SwapPieceResponse](result)
      newPiece.value must be >= 'A'
      newPiece.value must be <= 'Z'

      result = gameController.getFullGame(theGameId)(mkRequest(loginEvidence))
      val fullGameResponse: GetFullGameResponse = decodeJsonContent[GetFullGameResponse](result)
      val boardPiecePoints = fullGameResponse.boardPiecePoints;
      boardPiecePoints.size must equal (5)

      val userPiecePoints = userPlayPieces.map(_.piecePoint)

      userPiecePoints.foreach(piecePoint => {
        boardPiecePoints must contain (piecePoint)
      })

      logger.info(s"about to suspend game")

      result = gameController.suspendGame(theGameId)(mkRequest(loginEvidence))
      val unit = decodeJsonContent[Unit](result)

      result = gameController.getFullGame(theGameId)(mkRequest(loginEvidence))
      val fullGameResponseSuspended: GetFullGameResponse = decodeJsonContent[GetFullGameResponse](result)
      fullGameResponseSuspended.state must equal (GameState.SUSPENDED)
      fullGameResponseSuspended.boardPiecePoints must equal (fullGameResponse.boardPiecePoints)

      // Check cannot act on an inactive game.
      intercept[Exception] {
        result = gameController.closeGame(theGameId)(mkRequest(loginEvidence))
        val summary = decodeJsonContent[GameSummary](result)
        logger.info(s"end game result: ${summary}")
      }

      logger.info("about to resume game")
      result = gameController.resumeGame(theGameId)(mkRequest(loginEvidence))
      val resumeData = decodeJsonContent[Unit](result)

      logger.info("about to get full resumed game")
      result = gameController.getFullGame(theGameId)(mkRequest(loginEvidence))
      val fullGameResponseResumed: GetFullGameResponse = decodeJsonContent[GetFullGameResponse](result)
      fullGameResponseResumed must equal (fullGameResponse)

      logger.info("about to end game")
      result = gameController.closeGame(theGameId)(mkRequest(loginEvidence))
      val summary = decodeJsonContent[GameSummary](result)
      logger.info(s"end game result: ${summary}")
    }

    val isGuest: Boolean = true

    "play a mini game for real user" in {theTest(!isGuest)}
    // TODO. URGENT. Reinstate guest test. Note guest game cannot be suspended or resumed.
    // Fix test once the service code is fixed for guest games. Then reinstate.
    // "play a mini game for guest user" in {theTest(isGuest)}
  }
}
