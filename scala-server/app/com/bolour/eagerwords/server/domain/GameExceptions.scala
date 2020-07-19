/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.domain

object GameExceptions {

  sealed abstract class GameException(cause: Throwable = null) extends Exception(cause) {
    def causeMessage: String = if (cause == null) "" else s" - caused by: ${cause.getMessage}"
  }

  case class MissingPieceException(pieceId: String) extends GameException() {
    override def getMessage = s"piece does not exist for id: ${pieceId}"
  }

  case class MissingGameException(gameId: String) extends GameException() {
    override def getMessage = s"unable to find game - game has likely timed out: ${gameId}"
  }

  case class InactiveGameException(gameId: String) extends GameException() {
    override def getMessage = s"attempt to act on an inactive game: ${gameId}"
  }

  case class InvalidWordException(languageCode: String, word: String) extends GameException() {
    override def getMessage: String = s"'${word}' not found in the dictionary"
  }

  case class InvalidCrosswordsException(languageCode: String, crosswords: List[String]) extends GameException() {
    private val sep = ", "
    override def getMessage: String = s"crosswords [${crosswords.mkString(sep)}] not found in the dictionary"
  }

//  case class UnsupportedLanguageException(languageCode: String) extends GameException {
//    override def getMessage: String = s"unsupported language code: ${languageCode}"
//  }
//
//  case class MissingDictionaryException(languageCode: String, dictionaryDir: String, cause: Throwable) extends GameException(cause) {
//    override def getMessage: String = s"unable to read dictionary for language code: ${languageCode} from dictionary directory ${dictionaryDir}${causeMessage}"
//  }

  case class MalformedPlayException(condition: String) extends GameException {
    override def getMessage: String = s"malformed play - ${condition}"
  }
}
