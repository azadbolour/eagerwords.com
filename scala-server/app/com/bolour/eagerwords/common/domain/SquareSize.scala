/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

object SquareSize {

  val SmallString = "Small"
  val NormalString = "Normal"
  val LargeString = "Large"

  sealed abstract class SquareSize

  object SmallSquare extends SquareSize {
    override def toString = SmallString
  }

  object NormalSquare extends SquareSize {
    override def toString = NormalString
  }

  object LargeSquare extends SquareSize {
    override def toString = LargeString
  }

  def fromString(asString: String): SquareSize = {
    asString match {
      case SmallString => SquareSize.SmallSquare
      case NormalString => SquareSize.NormalSquare
      case LargeString => SquareSize.LargeSquare
      case _ => throw new RuntimeException(s"unrecognized square size: ${asString}")
    }
  }
}

