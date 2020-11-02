/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

import com.bolour.grid.domain.Point

/**
  * Combination of a piece and its location on the board.
  */
case class PiecePoint(piece: Piece, point: Point)
