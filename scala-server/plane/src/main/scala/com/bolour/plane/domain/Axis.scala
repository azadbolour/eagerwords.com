/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.plane.domain

/**
  * The X or Y axis of the plane.
  *
  * TODO. Enumeration is 'deprecated'. Use sealed abstract case class/objects.
  */
object Axis extends Enumeration {
  type Axis = Value
  val X, Y = Value

  def crossAxis(axis: Axis): Axis = if (axis == X) Y else X

  val forward = +1
  val backward = -1

}

