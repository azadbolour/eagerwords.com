/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.util

import scala.util.Try

class SymmetricCrypt(secret: String) {

  val javaCrypt: ISymmetricCrypt = new SymmetricCryptJava(secret)

  def encrypt(plainText: String): Try[String] = Try {
    javaCrypt.encrypt(plainText)
  }

  def decrypt(cipherText: String): Try[String] = Try {
    javaCrypt.decrypt(cipherText)
  }
}
