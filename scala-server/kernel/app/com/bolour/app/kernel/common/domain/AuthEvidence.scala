/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.common.domain

/**
  * Evidence of a user having been authenticated by passwordless authentication.
  *
  * @param clientId Unique identification of the calling user agent (aka client).
  *                 Returned directly to the client in response to a login request.
  * @param token Authentication token communicated to the user by a second factor.
  *              Initially email. Later also optionally as text.
  */
case class AuthEvidence(clientId: String, token: String);
