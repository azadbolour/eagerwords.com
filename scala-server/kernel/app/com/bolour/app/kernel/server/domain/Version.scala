/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.domain

/**
  * Version of the server.
  *
  * For now it serves as both the API version for clients,
  * and the persistence version for specific representations
  * of persisted objects.
  */
object Version {
  val version: Int = 1
}
