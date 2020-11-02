/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.grid.domain

import com.bolour.util.domain.BlackWhite

/**
  * A value at a location on a grid. Black means the location
  * is disabled. White means the location is enabled but may currently be empty.
  */
case class BlackWhitePoint[T](value: BlackWhite[T], point: Point)
