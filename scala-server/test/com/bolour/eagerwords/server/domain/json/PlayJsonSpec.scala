//package com.bolour.boardgame.server.domain.json
//
//import org.slf4j.LoggerFactory
//import spray.json._
//import com.bolour.boardgame.common.domain.PlayerType.MachinePlayer
//import org.scalatest.{FlatSpec, Matchers}
//import com.bolour.boardgame.server.domain._
//import com.bolour.boardgame.server.domain.json.CaseClassFormats._
//import com.bolour.boardgame.server.domain.json.PlayTypeJsonProtocol.PlayTypeFormat
//import com.bolour.boardgame.server.domain.json.PlayerTypeJsonProtocol.PlayerTypeFormat
//import com.bolour.boardgame.server.domain.json.PlayJsonProtocol.PlayJsonFormat
//import com.bolour.boardgame.common.domain.{Piece, PlayPiece}
//import com.bolour.plane.domain.Point
//class PlayJsonSpec extends FlatSpec with Matchers {
//
//  val logger = LoggerFactory.getLogger(this.getClass)
//
//  val playPieces = List(
//    PlayPiece(Piece('O', "1"), Point(0, 0), true),
//    PlayPiece(Piece('N', "2"), Point(0, 1), true)
//  )
//
//  val replacementPieces = List(
//    Piece('K', "3"), Piece('A', "4")
//  )
//
//  val deadPoints = List (Point(14, 14))
//  val wordPlay: Play = Play.mkWordPlay(1, MachinePlayer, List(0, 5), playPieces, replacementPieces, deadPoints)
//
//  "play type" should "be json convertible" in {
//    val json = PlayTypeFormat.write(WordPlayType)
//    val string = json.toString
//    logger.info(string)
//
//    val jsString = JsString(string)
//    val wordPlayType = PlayTypeFormat.read(jsString)
//
//    wordPlayType shouldEqual WordPlayType
//  }
//
//  "player type" should " be json convertible" in {
//    val json = PlayerTypeFormat.write(MachinePlayer)
//    val string = json.toString
//    logger.info(string)
//
//    val jsString = JsString(string)
//    val machinePlayer = PlayerTypeFormat.read(jsString)
//
//    machinePlayer shouldEqual MachinePlayer
//  }
//
//  "word play" should "be json convertible" in {
//
//    val json = wordPlay.toJson
//    val string = json.prettyPrint
//    logger.info(s"play as json string:\n${string}")
//
//    val jsonAst = string.parseJson
//    val wPlay: Play = jsonAst.convertTo[Play]
//
//    logger.info(s"${wPlay}")
//
//    wPlay shouldEqual wordPlay
//  }
//
//  "swap play" should "be json convertible" in {
//    val swappedPiece = Piece('O', "1")
//    val newPiece = Piece('N', "2")
//
//    val swapPlay: Play = Play.mkSwapPlay(1, MachinePlayer, List(0, 5), swappedPiece, newPiece)
//
//    val json = swapPlay.toJson
//    val string = json.prettyPrint
//    logger.info(s"play as json string:\n${string}")
//
//    val jsonAst = string.parseJson
//    val sPlay: Play = jsonAst.convertTo[Play]
//
//    logger.info(s"${sPlay}")
//
//    sPlay shouldEqual swapPlay
//  }
//
//}
