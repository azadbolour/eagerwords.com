/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.auth.server.service

import javax.inject.Inject
import com.typesafe.config.Config

class SecretServiceConfigImpl @Inject() (config: Config) extends SecretService {

  val emailPath = "testing.email"
  val tokenPath = "testing.token"

  val cipherIV = "cipher.iv"
  val encryptionKeyPath = "encryption.key"

  override def getTestingEmail: String = config.getString(emailPath)

  override def getTestingToken: String = config.getString(tokenPath)

  override def getEncryptionKey: String = config.getString(encryptionKeyPath)
}
