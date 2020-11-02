/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.language.domain

import java.io.File
import scala.io.Source
import scala.util.{Failure, Try}

import com.bolour.util.MiscUtil.{mkFileSource, mkResourceSource}
import com.bolour.language.domain.LanguageExceptions.MissingDictionaryException
import com.bolour.language.domain.WordDictionary._
import com.bolour.eagerwords.server.util.WordUtil._

/**
  * Word dictionary - indexed by combinations of letters in a word.
  * A combination is represented as the sorted string of letters.
  *
  * The dictionary also includes an index of "dense" masked words. A masked word
  * is a word some of whose letters have been changed to blanks (for the purpose
  * of matching with the contents of strips on the board). If a strip
  * is at all playable, then its content as a masked word must exist in the
  * masked words index. However, we do not store all masked versions of
  * a word: only those that are "dense", that is, those that only have a few
  * blanks. This index is used in identifying blanks that cannot possibly
  * be filled, because their eligible play strips are all dense but have contents
  * that do not exist as masked words in the masked word index.
  *
  * The class MaskedWordsPreprocessor is used to pre-compute dense masked words.
  *
  * Dictionary files reside in the "dict" directory. The words file is named
  * "${languageCode}${dictionaryFileSuffix}", where languageCode is the locale
  * code for the language, e.g., "en", and dictionaryFileSuffix = "-words.txt".
  * Similarly, the masked words file is named "${languageCode}${maskedWordsFileSuffix}",
  * where maskedWordsFileSuffix = "-masked-words.txt".
  */
case class WordDictionary(languageCode: String, words: List[DictWord], maskedWords: Set[String], maxMaskedLetters: Int) {
// case class WordDictionary(languageCode: String, words: List[DictWord], maskedWords: CompactStringSet, maxMaskedLetters: Int) {

  /**
    * Map of letter combinations to words in the dictionary.
    */
  private val wordsByCombo = mkWordsByCombo(words)

  /**
    * Is the given word in the dictionary?
    */
  def hasWord(word: String): Boolean = permutations(stringToLetterCombo(word)) contains word

  def hasMaskedWord(maskedWord: String): Boolean = maskedWords.contains(maskedWord)

  /**
    * Get the words in the dictionary that have exactly the same letters
    * as the given sorted list of letter.
    * */
  def permutations(combo: LetterCombo): List[DictWord] = wordsByCombo.getOrElse(combo, Nil)
}

object WordDictionary {

  def mkWordDictionary(languageCode: String, dictionaryDir: String, maxMaskedLetters: Int): Try[WordDictionary] = {
    for {
      words <- readDictionary(languageCode, dictionaryDir)
      maskedWords <- readMaskedWordsCompact(languageCode, dictionaryDir)
    } yield WordDictionary(languageCode, words, maskedWords, maxMaskedLetters)
  }

  private val classLoader = this.getClass.getClassLoader

  val dictionaryFileSuffix = "-words.txt"
  val maskedWordsFileSuffix = "-masked-words.txt"

  private def dictionaryFileName(languageCode: String): String =
    s"${languageCode}${dictionaryFileSuffix}"

  private def maskedWordsFileName(languageCode: String): String =
    s"${languageCode}${maskedWordsFileSuffix}"

  private def mkDictionarySourceFromPath(languageCode: String, path: String): Try[Source] = {
    mkFileSource(path)
      .orElse(mkResourceSource(path, WordDictionary.classLoader))
      .recoverWith({
        case th: Throwable => Failure(MissingDictionaryException(languageCode, path, th))
      })
  }

  def readDictionary(languageCode: String, dictionaryDir: String): Try[List[DictWord]] = {
    val name = dictionaryFileName(languageCode)
    val path = s"${dictionaryDir}${File.separator}${name}"
    logger.info(s"dictionary path: $path")
    for {
      // source <- mkFileSource(path).orElse(mkResourceSource(path, WordDictionary.classLoader))
      source <- mkDictionarySourceFromPath(languageCode, path)
      words <- Try {
        // TODO. Better pattern for closing source?
        try {
          val lines = source.getLines().toList
          logger.info(s"words in dictionary: ${lines.size}")
          lines.map(_.toUpperCase)
        }
        finally {source.close}
      }
    } yield words
  }

  def readMaskedWords(languageCode: String, dictionaryDir: String): Try[Set[String]] = {
    val name = maskedWordsFileName(languageCode)
    val path = s"${dictionaryDir}${File.separator}${name}"
    for {
      // source <- mkFileSource(path).orElse(mkResourceSource(path, WordDictionary.classLoader))
      source <- mkDictionarySourceFromPath(languageCode, path)
      maskedWords <- Try {
        try {
          val lines = source.getLines().toSet
          lines.map(_.toUpperCase)
        }
        finally {source.close}
      }
    } yield maskedWords
  }

  def readMaskedWordsCompact(languageCode: String, dictionaryDir: String): Try[Set[String]] = {
    val name = maskedWordsFileName(languageCode)
    val path = s"${dictionaryDir}${File.separator}${name}"
    for {
      // source <- mkFileSource(path).orElse(mkResourceSource(path, WordDictionary.classLoader))
      source <- mkDictionarySourceFromPath(languageCode, path)
      set <- Try {
        try {
          val lines = source.getLines()
          val uppers = lines.map(_.toUpperCase)
          // CompactStringSet(uppers)
          uppers.toSet
        }
        finally {source.close}
      }
    } yield set
  }

  /**
    * Create a map of letter combinations to words in the dictionary.
    * The combinations of letters in a word is represented by the sorted
    * list of the letters in teh word.
    */
  def mkWordsByCombo(words: List[DictWord]): WordsByCombo = words.groupBy(stringToLetterCombo)

  /**
    * Given the words in the dictionary, created the set of all dense masked versions
    * of the words. A dense masked version of the word has up to "maxMaskedLetters"
    * of its letters "masked" (that is replaced) by blanks.
    */
  def mkMaskedWords(words: List[DictWord], maxMaskedLetters: Int): Set[String] = {
    val list = for {
      word <- words
      masked <- maskWithBlanks(word, maxMaskedLetters)
    } yield masked
    list.toSet
  }

  def mkMaskedWordsCompact(words: List[DictWord], maxMaskedLetters: Int): Set[String] = {
    val list = for {
      word <- words
      masked <- maskWithBlanks(word, maxMaskedLetters)
    } yield masked
    list.toSet
  }
}

