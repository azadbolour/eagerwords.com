/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.common.message

/**
  * Response to an initial sign-up request.
  *
  * @param clientId Unique id assigned by the server to the user agent (client).
  */
case class InitSignUpResponse(clientId: String)
