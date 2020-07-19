/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.plane.domain

import com.bolour.plane.domain.Axis.Axis

/**
  * A point on the plane.
  */
case class Point(row: Int, col: Int) {
  def colinearPoint(axis: Axis, direction: Int)(steps: Int): Point = {
    val offset = direction * steps
    axis match {
      case Axis.X => Point(row, col + offset)
      case Axis.Y => Point(row + offset, col)

    }
  }

  def adjPoint(axis: Axis, direction: Int): Point = colinearPoint(axis, direction)(1)
}

