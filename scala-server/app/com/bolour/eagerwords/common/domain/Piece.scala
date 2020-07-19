/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.domain

/**
  * A piece (tile) used in the game.
  *
  * @param value An upper-case alphabetic character.
  * @param id The unique identifier of the piece within a game.
  */
case class Piece(value: Char, id: String)
