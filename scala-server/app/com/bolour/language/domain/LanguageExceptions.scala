/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.language.domain

object LanguageExceptions {
  sealed abstract class LanguageException(cause: Throwable = null) extends Exception(cause) {
    def causeMessage: String = if (cause == null) "" else s" - caused by: ${cause.getMessage}"
  }

  case class MissingDictionaryException(languageCode: String, dictionaryDir: String, cause: Throwable) extends LanguageException(cause) {
    override def getMessage: String = s"unable to read dictionary for language code: ${languageCode} from dictionary directory ${dictionaryDir}${causeMessage}"
  }

  case class UnsupportedLanguageException(languageCode: String) extends LanguageException {
    override def getMessage: String = s"unsupported language code: ${languageCode}"
  }



}
