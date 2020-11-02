/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.auth.server.service

trait SecretService {
  /**
    * Get the special email used for integration tests.
    * Known secretly to integration tests and to the test server.
    * Prevents email from being sent in tests.
    */
  def getTestingEmail: String

  /**
    * Get the special authentication token used for integration tests.
    * Known secretly to integration tests and to the test server.
    */
  def getTestingToken: String

  /**
    * Get the encryption key.
    */
  def getEncryptionKey: String

}
