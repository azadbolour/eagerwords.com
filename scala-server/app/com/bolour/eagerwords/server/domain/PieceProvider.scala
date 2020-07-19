/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.server.domain

import com.bolour.eagerwords.common.domain.{Piece, PieceProviderType}
import com.bolour.eagerwords.common.domain.PieceProviderType.PieceProviderType
import com.bolour.util.FrequencyDistribution

import scala.util.{Success, Try}

/**
  * Factory for generating new pieces as pieces are consumed in plays.
  *
  * Note. Piece provider properties are transitive properties of games,
  * and need to be saved to and stored in the database. Hence no side effects.
  */
sealed abstract class PieceProvider {

  def take(): Try[(PieceProvider, Piece)]

  def takePieces(num: Int): Try[(PieceProvider, List[Piece])] = takePiecesAux(List(), num)

  def takePiecesAux(pieces: List[Piece], n: Int): Try[(PieceProvider, List[Piece])] = {
    if (n == 0)
      return Success((this, pieces))
    for {
      (provider1, piece) <- take()
      (provider2, pieces) <- provider1.takePiecesAux(piece +: pieces, n - 1)
    } yield (provider2, pieces)
  }

  def swapOne(piece: Piece): Try[(PieceProvider, Piece)] = {
    for {
      (provider1, resultPiece) <- this.take()
    } yield (provider1, resultPiece)
  }
}

/**
  * Cyclic piece generator - cycles through a given list of letters generating successive pieces.
  *
  * @param valueList The list of letters to cycle through.
  */
case class CyclicPieceProvider(valueList: String, counter: BigInt) extends PieceProvider {
  if (valueList == null || valueList.isEmpty)
    throw new IllegalArgumentException("empty cyclic generator value list")

  override def take(): Try[(PieceProvider, Piece)] = Try {
    val cycleLength: Int = valueList.length
    val index: Int = (counter % cycleLength).toInt
    val value = valueList(index)
    val id = counter.toString
    val piece = Piece(value, id)
    val nextPieceProvider = CyclicPieceProvider(valueList, counter + 1)
    (nextPieceProvider, piece)
  }
}

//object CyclicPieceProvider {
//  def apply() = new CyclicPieceProvider("ABCDEFGHIJKLMNOPQRSTUVWXYZ", 0)
//}

/**
  * Random piece generator - generate pieces with random letters.
  */
case class RandomPieceProvider(letterDistribution: FrequencyDistribution[Char], counter: BigInt) extends PieceProvider {

  def take(): Try[(RandomPieceProvider, Piece)] = Try {
    val letter = letterDistribution.randomValue()
    val id = counter.toString
    val piece = Piece(letter, id)
    val nextPieceProvider = RandomPieceProvider(letterDistribution, counter + 1)
    (nextPieceProvider, piece)
  }
}

//object RandomPieceProvider {
//  def apply(letterDistribution: FrequencyDistribution[Char]) = new RandomPieceProvider(letterDistribution, 0)
//}


