/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.auth.server.domain

/**
  * Information about an on-going (as-yet-unconfirmed) sign-up request.
  *
  * @param email The requester's email.
  * @param nickname User's nickname.
  * @param clientId Unique id of the requester's client program.
  * @param token Authentication token send by email to the user for confirmation.
  * @param expiration Epoch seconds at which time this sign-up request expires.
  */
case class SignUp(email: String, nickname: String, clientId: String, token: String, expiration: Long)
