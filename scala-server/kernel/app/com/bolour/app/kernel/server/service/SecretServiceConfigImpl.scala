/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.service

import com.typesafe.config.Config
import javax.inject.Inject

class SecretServiceConfigImpl @Inject() (config: Config) extends SecretService {

  val emailPath = "testing.email"
  val tokenPath = "testing.token"

  val cipherIV = "cipher.iv"
  val encryptionKeyPath = "encryption.key"

  override def getTestingEmail: String = config.getString(emailPath)

  override def getTestingToken: String = config.getString(tokenPath)

  override def getEncryptionKey: String = config.getString(encryptionKeyPath)
}
