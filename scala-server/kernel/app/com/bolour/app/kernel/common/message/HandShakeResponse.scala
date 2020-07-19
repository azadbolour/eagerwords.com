/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.common.message

/**
  * API response for the initial handshake.
  *
  * @param serverType - Scala, ...
  * @param apiVersion
  */
case class HandShakeResponse(
  serverType: String,
  apiVersion: String
)
