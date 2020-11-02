/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.message

/**
  * Data transfer objects for errors. Disentangle API errors from internal
  * implementation errors.
  */
sealed abstract class GameErrorDto

case class MissingPieceErrorDto(tag: String, message: String, pieceId: String) extends GameErrorDto

case class MissingGameErrorDto(tag: String, message: String, gameId: String) extends GameErrorDto

case class InactiveGameErrorDto(tag: String, message: String, gameId: String) extends GameErrorDto

case class InvalidWordErrorDto(tag: String, message: String, languageCode: String, word: String) extends GameErrorDto

case class InvalidCrosswordsErrorDto(tag: String, message: String, languageCode: String, crossWords: List[String]) extends GameErrorDto

case class UnsupportedLanguageErrorDto(tag: String, message: String, languageCode: String)

case class MissingDictionaryErrorDto(tag: String, message: String, languageCode: String, dictionaryDir: String) extends GameErrorDto

case class MalformedPlayErrorDto(tag: String, message: String, condition: String) extends GameErrorDto

case class IllegalGuestOpErrorDto(tag: String, message: String, op: String) extends GameErrorDto

case class CompletedGameOpErrorDto(tag: String, message: String, op: String) extends GameErrorDto
