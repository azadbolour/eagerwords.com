/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.domain

import com.bolour.eagerwords.server.util.WordUtil
import com.bolour.plane.domain.Axis._
import com.bolour.plane.domain.Axis.Axis
import com.bolour.eagerwords.server.util.WordUtil._
import com.bolour.plane.domain.{Axis, Point}
import com.bolour.util.BlackWhite

/**
  * A strip of the board - a horizontal or vertical line segment.
  *
  * @param axis X or Y for horizontal or vertical.
  * @param lineNumber The index of the line on the board - for example 0 for the first row.
  * @param begin The offset within the given line where the strip begins.
  * @param end The offset within the given line where the strip ends (inclusive).
  * @param content The content of the strip - sequence of letters and blanks.
  */
case class Strip(
  axis: Axis,
  lineNumber: Int,
  begin: Int,
  end: Int,
  content: String
) {

  /**
    * Combination of letters on strip (sorted - with dups).
    */
  val letters: LetterCombo = stringToLetterCombo(content.filter(_ != blankChar))

  /**
    * Number of blank slots on strip.
    */
  val numBlanks: Int = content.length - letters.length

  val len: Int = end - begin + 1

  /**
    * Word can potentially be played to this strip.
    * They have the same length, and the non-blank slots
    * of the strip match the corresponding letter of the word.
    */
  def admits(word: String): Boolean = {
    (len == word.length) && fits(content, word)
  }

  def findFittingWords(words: List[DictWord]): List[DictWord] = {
    words.filter(admits)
  }

  def findFittingWord(words: List[DictWord]): Option[DictWord] =
    words.find(admits)

  def hasAnchor: Boolean = numBlanks < len

  def isDense(maxBlanks: Int): Boolean = hasAnchor && numBlanks <= maxBlanks

  def row(offset: Int): Int = axis match {
    case X => lineNumber
    case Y => begin + offset
  }

  def column(offset: Int): Int = axis match {
    case X => begin + offset
    case Y => lineNumber
  }

  def point(offset: Int) = Point(row(offset), column(offset))

  def offset(point: Point): Int = {
    val indexInLine = axis match {
      case Axis.X => point.col
      case Axis.Y => point.row
    }
    indexInLine - begin
  }

  def fillBlankInStrip(point: Point, ch: Char): String = content.updated(offset(point), ch)

  // TODO. Use fold.
  /**
    * It has already been established that the rest of the word has the same length
    * as the rest of the strip content - so just compare their corresponding letters
    */
  private def fits(restContent: String, restWord: String): Boolean =
    if (restWord.isEmpty) true
    else Strip.fitsSlot(restContent.head, restWord.head) &&
           fits(restContent.tail, restWord.tail)

  def blankPoints: List[Point] = {
    val blankOffsets = content.indices.toList.filter(offset => WordUtil.isBlankChar(content(offset)))
    blankOffsets map point
  }
}

object Strip {

  def lineStrip(axis: Axis, lineNumber: Int, line: String, begin: Int, end: Int): Strip = {
    val content = line.slice(begin, end + 1)
    Strip(axis, lineNumber, begin, end, content)
  }

  def fitsSlot(slotLetter: Char, wordLetter: Char): Boolean =
    slotLetter == ' ' || slotLetter == wordLetter

  def stripFromBlackWhiteLine(axis: Axis, lineNumber: Int, blackWhiteChars: List[BlackWhite[Char]], offset: Int, size: Int): Strip = {
    val lineAsString = (blackWhiteChars map blackWhiteToChar).mkString
    lineStrip(axis, lineNumber, lineAsString, offset, offset + size - 1)
  }

  def stripsInLine(axis: Axis, lineNumber: Int, chars: String): List[Strip] = {
    val dimension = chars.length
    for {
      offset <- (0 until dimension).toList
      end <- offset until dimension
    } yield lineStrip(axis, lineNumber, chars, offset, end)
  }

  def allStripsInBlackWhiteLine(axis: Axis, lineNumber: Int, line: List[BlackWhite[Char]]): List[Strip] = {
    val lineAsString = (line map blackWhiteToChar).mkString
    stripsInLine(axis, lineNumber, lineAsString)
  }

  def blackWhiteToChar(blackWhiteChar: BlackWhite[Char]): Char =
    blackWhiteChar.toValueWithDefaults(blackChar, blankChar)
}

