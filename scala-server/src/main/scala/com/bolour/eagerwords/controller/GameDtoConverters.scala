package com.bolour.eagerwords.controller

import com.bolour.language.domain.LanguageExceptions.{MissingDictionaryException, UnsupportedLanguageException}
import com.bolour.eagerwords.common.domain.PlayerType.{playerIndex, MachinePlayer, UserPlayer}
import com.bolour.eagerwords.common.message._
import com.bolour.eagerwords.server.domain.Game
import com.bolour.eagerwords.server.domain.GameExceptions._

object GameDtoConverters {

  def mkStartGameResponse(game: Game): StartGameResponse = {
    val piecePoints = game.board.piecePoints
    val userTray = game.trays(playerIndex(UserPlayer))
    StartGameResponse(game.gameBase.gameId, piecePoints, userTray.pieces.toList)
  }

  def mkResumeGameResponse(game: Game): ResumeGameResponse = {
    val base = game.gameBase;
    val piecePoints = game.board.piecePoints
    val userTray = game.trays(playerIndex(UserPlayer))
    ResumeGameResponse(base.gameId, base.gameParams, piecePoints, userTray.pieces.toList, game.scores(0), game.scores(1))
  }

  def mkGetFullGameResponse(game: Game): GetFullGameResponse = {
    val base = game.gameBase;
    val piecePoints = game.board.piecePoints
    val trayPieces = game.trays(playerIndex(UserPlayer)).pieces.toList
    val board = game.board

    GetFullGameResponse(
      base.gameId,
      base.gameParams,
      piecePoints,
      trayPieces,
      game.scores(0),
      game.scores(1),
      game.playNumber,
      game.playTurn,
      game.plays,
      game.state,
      board.deadPoints()
    )
  }

  def mkGetUserGamesResponse(games: List[Game]): GetGamesResponse = {
    val infoList = games.map(gameToBasicGameInfo)
    GetGamesResponse(infoList)
  }

  def gameToBasicGameInfo(game: Game): GameBasicInfo = {
    GameBasicInfo(
      game.gameBase.gameId,
      game.gameBase.firstSecond,
      game.lastSecond,
      game.state.toString,
      game.scores(playerIndex(UserPlayer)),
      game.scores(playerIndex(MachinePlayer))
    )
  }

  // LanguageExceptions.

  def toUnsupportedLanguageErrorDto(ex: UnsupportedLanguageException) =
    UnsupportedLanguageErrorDto("UnsupportedLanguageError", ex.getMessage, ex.languageCode)

  def toMissingDictionaryErrorDto(ex: MissingDictionaryException) =
    MissingDictionaryErrorDto("MissingDictionaryError", ex.getMessage, ex.languageCode, ex.path)

  // GameExceptions.

  // def fromUserDto(dto: UserDto): User = domain.User(dto.id, dto.userId, dto.name, dto.email)

  def toMissingPieceErrorDto(ex: MissingPieceException) =
    MissingPieceErrorDto("MissingPieceError", ex.getMessage, ex.pieceId)

  def toMissingGameErrorDto(ex: MissingGameException) =
    MissingGameErrorDto("MissingGameError", ex.getMessage, ex.gameId)

  def toInactiveGameErrorDto(ex: InactiveGameException) =
    InactiveGameErrorDto("InactiveGameError", ex.getMessage, ex.gameId)

  def toInvalidWordErrorDto(ex: InvalidWordException) =
    InvalidWordErrorDto("InvalidWordError", ex.getMessage, ex.languageCode, ex.word)

  def toInvalidCrosswordsErrorDto(ex: InvalidCrosswordsException) =
    InvalidCrosswordsErrorDto("InvalidCrosswordsError", ex.getMessage, ex.languageCode, ex.crosswords)

  def toMalformedPlayErrorDto(ex: MalformedPlayException) =
    MalformedPlayErrorDto("MalformedPlayError", ex.getMessage, ex.condition)

  def toIllegalGuestOpErrorDto(ex: IllegalGuestOpException) =
    IllegalGuestOpErrorDto("IllegalGuestOp", ex.getMessage, ex.op)

  def toCompletedGameOpErrorDto(ex: CompletedGameOpException) =
    CompletedGameOpErrorDto("CompletedGameOp", ex.getMessage, ex.op)

}
