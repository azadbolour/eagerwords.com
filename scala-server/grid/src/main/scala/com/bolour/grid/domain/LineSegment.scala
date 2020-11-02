/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.grid.domain

import com.bolour.grid.domain.Axis.{Axis, X, Y}

/**
  * An active line segment of a grid - a line segment does not contain
  * inactive (black) points.
  *
  * @param axis The direction of the line segment (X, Y).
  * @param lineNumber The index of the enclosing grid line (row for X, col for Y).
  * @param begin The starting position of the segment in its line.
  * @param end The ending position of the segment in its line.
  * @param segment The list of values (or no-values) of the segment.
  * @tparam T The type of the contained value if any.
  */
case class LineSegment[T](axis: Axis, lineNumber: Int, begin: Int, end: Int, segment: List[Option[T]]) {

  def row(offset: Int): Int = axis match {
    case X => lineNumber
    case Y => begin + offset
  }

  def column(offset: Int): Int = axis match {
    case X => begin + offset
    case Y => lineNumber
  }

  def pointAt(offset: Int) = Point(row(offset), column(offset))
}


