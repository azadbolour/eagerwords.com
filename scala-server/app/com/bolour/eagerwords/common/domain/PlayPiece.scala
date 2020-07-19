/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

import com.bolour.plane.domain.Point

case class PlayPiece(piece: Piece, point: Point, moved: Boolean) {
  def piecePoint = PiecePoint(piece, point)
}

// Companion object renamed to avoid akka http json format issue.

object PlayPieceObj {
  type PlayPieces = List[PlayPiece]
  def playPiecesToWord(playPieces: List[PlayPiece]): String =
    (playPieces map { _.piece.value }).mkString
}
