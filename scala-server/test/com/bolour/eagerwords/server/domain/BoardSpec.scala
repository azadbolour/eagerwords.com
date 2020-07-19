/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.domain

import org.slf4j.LoggerFactory
import org.scalatest.{FlatSpec, Matchers}
import com.bolour.eagerwords.common.domain.{Piece, PiecePoint}
import com.bolour.plane.domain.Point

class BoardSpec extends FlatSpec with Matchers {
  val logger = LoggerFactory.getLogger(this.getClass)

  "board" should "start empty" in {
    val board1 = Board(15)
    board1.piecePoints.size shouldEqual 0

    val noPiecePoints: List[PiecePoint] = Nil
    val board2: Board = Board(2, noPiecePoints)
    board2.piecePoints.size shouldBe 0

    val piecePoints = List(PiecePoint(Piece('A', "idA"), Point(0, 0)))
    val board3 = Board(1, piecePoints)
    logger.info(s"${board3}")
    board3.piecePoints.size shouldEqual 1
  }
}
