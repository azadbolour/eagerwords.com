/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.domain

import com.bolour.util.TimeUtil.nowSecs

/**
  * Information about an on-going (as-yet-unconfirmed) login request.
  *
  * @param email The requester's email.
  * @param clientId Unique id of the requester's client program.
  * @param token Authentication token send by email to the user for confirmation.
  * @param confirmed Has the login been confirmed via the authentication token sent to the user.
  * @param expiration Epoch seconds at which time the login expires.
  * @param confirmExpiration The expiration of an unconfirmed login.
  */
case class Login(email: String, clientId: String, token: String, confirmed: Boolean, expiration: Long, confirmExpiration: Long) {
  def isLoggedIn: Boolean = confirmed && (expiration > nowSecs)
}
