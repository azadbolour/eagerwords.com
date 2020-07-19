/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.util

/**
  * Add a version number to a data structure.
  *
  * There would be specific encoding/decoding methods for each version
  * of the data structure, allowing the encodings to change over time,
  * but old encodings to still be decodable.
  *
  * @param version The version number.
  * @param data The data structure.
  */
case class VersionStamped[T](version: Int, data: T)
