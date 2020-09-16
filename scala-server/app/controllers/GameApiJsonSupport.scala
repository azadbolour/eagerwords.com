/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package controllers

import com.bolour.eagerwords.common.domain.DeviceType.DeviceType
import com.bolour.eagerwords.common.domain.GameState.GameState
import com.bolour.eagerwords.common.domain.PieceProviderType._
import com.bolour.eagerwords.common.domain.PlayerType.PlayerType
import controllers.KernelApiJsonSupport._
import play.api.libs.json.Json.{reads, writes}
import com.bolour.eagerwords.common.message._
import com.bolour.eagerwords.common.domain.{PieceProviderType, _}
import com.bolour.eagerwords.common.domain.Play.PlayTypeFieldName
import com.bolour.eagerwords.common.domain.PlayType._
import com.bolour.eagerwords.common.domain.SquareSize.SquareSize
import com.bolour.plane.domain.Point
import play.api.libs.json._

/**
  * Implicit JSON readers and writers for game dto objects.
  */
object GameApiJsonSupport {

  implicit val gameStateReads: Reads[GameState] = new Reads[GameState] {
    def reads(json: JsValue): JsResult[GameState] = {
      val string = Json.fromJson[String](json)
      string map GameState.fromString
    }
  }

  implicit val pieceProviderTypeReads: Reads[PieceProviderType] = new Reads[PieceProviderType] {
    def reads(json: JsValue): JsResult[PieceProviderType] = {
      val string = Json.fromJson[String](json)
      string map PieceProviderType.fromString
    }
  }

  implicit val playTypeReads: Reads[PlayType] = new Reads[PlayType] {
    def reads(json: JsValue): JsResult[PlayType] = {
      val string = Json.fromJson[String](json)
      string map PlayType.fromString
    }
  }

  implicit val playerTypeReads: Reads[PlayerType] = new Reads[PlayerType] {
    def reads(json: JsValue): JsResult[PlayerType] = {
      val string = Json.fromJson[String](json)
      string map PlayerType.fromString
    }
  }

  implicit val deviceTypeReads: Reads[DeviceType] = new Reads[DeviceType] {
    def reads(json: JsValue): JsResult[DeviceType] = {
      val string = Json.fromJson[String](json)
      string map DeviceType.fromString
    }
  }

  implicit val squareSizeReads: Reads[SquareSize] = new Reads[SquareSize] {
    def reads(json: JsValue): JsResult[SquareSize] = {
      val string = Json.fromJson[String](json)
      string map SquareSize.fromString
    }
  }

  implicit val gameLookAndFeelParamsReads: Reads[GameLookAndFeelParams] = reads[GameLookAndFeelParams]
  implicit val gamePlayParamsReads: Reads[GamePlayParams] = reads[GamePlayParams]
  implicit val gameSettingsReads: Reads[UserGameSettings] = reads[UserGameSettings]
  implicit val gameParamsReads: Reads[GameParams] = reads[GameParams]
  implicit val pieceReads: Reads[Piece] = reads[Piece]
  implicit val pointReads: Reads[Point] = reads[Point]
  implicit val piecePointReads: Reads[PiecePoint] = reads[PiecePoint]
  implicit val initPiecesReads: Reads[InitPieces] = reads[InitPieces]
  implicit val gameMiniStateReads: Reads[GameMiniState] = reads[GameMiniState]
  implicit val startGameResponseReads: Reads[StartGameResponse] = reads[StartGameResponse]
  implicit val startGameRequestReads: Reads[StartGameRequest] = reads[StartGameRequest]
  implicit val getUserGamesRequestReads: Reads[GetUserGamesRequest] = reads[GetUserGamesRequest]
  implicit val saveUserGameSettingsRequestReads: Reads[SaveUserGameSettingsRequest] = reads[SaveUserGameSettingsRequest]
  implicit val playPieceReads: Reads[PlayPiece] = reads[PlayPiece]
  implicit val commitPlayResponseReads: Reads[CommitPlayResponse] = reads[CommitPlayResponse]
  implicit val resumeGameResponseReads: Reads[ResumeGameResponse] = reads[ResumeGameResponse]
  implicit val machinePlayResponseReads: Reads[MachinePlayResponse] = reads[MachinePlayResponse]
  implicit val swapPieceResponseReads: Reads[SwapPieceResponse] = reads[SwapPieceResponse]
  implicit val stopInfoReads: Reads[StopInfo] = reads[StopInfo]
  implicit val gameSummaryReads: Reads[GameSummary] = reads[GameSummary]
  implicit val commitPlayRequestReads: Reads[CommitPlayRequest] = reads[CommitPlayRequest]
  implicit val swapPieceRequestReads: Reads[SwapPieceRequest] = reads[SwapPieceRequest]

  implicit val wordPlayReads: Reads[WordPlay] = reads[WordPlay]
  implicit val swapPlayReads: Reads[SwapPlay] = reads[SwapPlay]

  implicit val playReads: Reads[Play] = new Reads[Play] {
    def reads(json: JsValue): JsResult[Play] = {
      json match {
        case jsonObject: JsObject =>
          val playType: String = jsonObject.value.getOrElse(PlayTypeFieldName, JsNull).toString().replaceAll("\"", "")
          playType match {
            case WordString => jsonObject.validate[WordPlay]
            case SwapString => jsonObject.validate[SwapPlay]
            case _ => JsError(s"unrecognized play type: ${playType}")
          }
        case _ => JsError("expected field playType not found in json")
      }
    }
  }

  implicit val getFullGameResponseReads: Reads[GetFullGameResponse] = reads[GetFullGameResponse]

  implicit val gameStateWrites: Writes[GameState] = new Writes[GameState] {
    def writes(o: GameState): JsValue = Json.toJson[String](o.toString)
  }

  implicit val pieceProviderTypeWrites: Writes[PieceProviderType] = new Writes[PieceProviderType] {
    def writes(o: PieceProviderType): JsValue = Json.toJson[String](o.toString)
  }

  implicit val playTypeWrites: Writes[PlayType] = new Writes[PlayType] {
    def writes(o: PlayType): JsValue = Json.toJson[String](o.toString)
  }

  implicit val playerTypeWrites: Writes[PlayerType] = new Writes[PlayerType] {
    def writes(o: PlayerType): JsValue = Json.toJson[String](o.toString)
  }

  implicit val deviceTypeWrites: Writes[DeviceType] = new Writes[DeviceType] {
    def writes(o: DeviceType): JsValue = Json.toJson[String](o.toString)
  }

  implicit val squareSizeWrites: Writes[SquareSize] = new Writes[SquareSize] {
    def writes(o: SquareSize): JsValue = Json.toJson[String](o.toString)
  }

  implicit val gameLookAndFeelParamsWrites: OWrites[GameLookAndFeelParams] = writes[GameLookAndFeelParams]
  implicit val gamePlayParamsWrites: OWrites[GamePlayParams] = writes[GamePlayParams]
  implicit val gameSettingsWrites: OWrites[UserGameSettings] = writes[UserGameSettings]
  implicit val gameParamsWrites: OWrites[GameParams] = writes[GameParams]
  implicit val gameBasicInfoWrites: OWrites[GameBasicInfo] = writes[GameBasicInfo]
  implicit val pieceWrites: OWrites[Piece] = writes[Piece]
  implicit val pointWrites: OWrites[Point] = writes[Point]
  implicit val piecePointWrites: OWrites[PiecePoint] = writes[PiecePoint]
  implicit val initPiecesWrites: OWrites[InitPieces] = writes[InitPieces]
  implicit val gameMiniStateWrites: OWrites[GameMiniState] = writes[GameMiniState]
  implicit val startGameResponseWrites: OWrites[StartGameResponse] = writes[StartGameResponse]
  implicit val getGamesResponseWrites: OWrites[GetGamesResponse] = writes[GetGamesResponse]
  implicit val saveUserGameSettingsRequestWrites: OWrites[SaveUserGameSettingsRequest] = writes[SaveUserGameSettingsRequest]
  implicit val resumeGameResponseWrites: OWrites[ResumeGameResponse] = writes[ResumeGameResponse]
  implicit val startGameRequestWrites: OWrites[StartGameRequest] = writes[StartGameRequest]
  implicit val playPieceWrites: OWrites[PlayPiece] = writes[PlayPiece]
  implicit val commitPlayResponseWrites: OWrites[CommitPlayResponse] = writes[CommitPlayResponse]
  implicit val machinePlayResponseWrites: OWrites[MachinePlayResponse] = writes[MachinePlayResponse]
  implicit val swapPieceResponseWrites: OWrites[SwapPieceResponse] = writes[SwapPieceResponse]
  implicit val stopInfoWrites: OWrites[StopInfo] = writes[StopInfo]
  implicit val gameSummaryWrites: OWrites[GameSummary] = writes[GameSummary]
  implicit val commitPlayRequestWrites: OWrites[CommitPlayRequest] = writes[CommitPlayRequest]
  implicit val swapPieceRequestWrites: OWrites[SwapPieceRequest] = writes[SwapPieceRequest]

  implicit val missingPieceErrorDtoWrites: OWrites[MissingPieceErrorDto] = writes[MissingPieceErrorDto]
  implicit val missingGameErrorDtoWrites: OWrites[MissingGameErrorDto] = writes[MissingGameErrorDto]
  implicit val inactiveGameErrorDtoWrites: OWrites[InactiveGameErrorDto] = writes[InactiveGameErrorDto]
  implicit val invalidWordErrorDtoWrites: OWrites[InvalidWordErrorDto] = writes[InvalidWordErrorDto]
  implicit val invalidCrosswordsErrorDtoWrites: OWrites[InvalidCrosswordsErrorDto] = writes[InvalidCrosswordsErrorDto]
  implicit val unsupportedLanguageErrorDtoWrites: OWrites[UnsupportedLanguageErrorDto] = writes[UnsupportedLanguageErrorDto]
  implicit val missingDictionaryErrorDtoWrites: OWrites[MissingDictionaryErrorDto] = writes[MissingDictionaryErrorDto]
  implicit val malformedPlayErrorDtoWrites: OWrites[MalformedPlayErrorDto] = writes[MalformedPlayErrorDto]

  implicit val wordPlayWrites = writes[WordPlay]
  implicit val swapPlayWrites = writes[SwapPlay]

  implicit val playWrites = new Writes[Play] {
    def writes(play: Play): JsValue =
      play match {
        case wordPlay: WordPlay => Json.toJson(wordPlay)
        case swapPlay: SwapPlay => Json.toJson(swapPlay)
      }
  }

  implicit val getFullGameResponseWrites: OWrites[GetFullGameResponse] = writes[GetFullGameResponse]
}
