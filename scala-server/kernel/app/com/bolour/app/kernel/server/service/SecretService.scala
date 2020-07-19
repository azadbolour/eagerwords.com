/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.service

trait SecretService {
  /**
    * Get the special email used for integration tests.
    * Known secretly to integration tests and to the test server.
    * Prevents email from being sent in tests.
    */
  def getMockEmail: String

  /**
    * Get the special authentication token used for integration tests.
    * Known secretly to integration tests and to the test server.
    */
  def getMockToken: String

  /**
    * Get the encryption key.
    */
  def getEncryptionKey: String

}
