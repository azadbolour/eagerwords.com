/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.message

import com.bolour.app.kernel.common.domain.AuthEvidence

/**
  * API request to get basic information about a set of games belonging to a player.
  *
  * The number of games retrieved is the minimum of the maxGames parameter if given,
  * and the absolute maximum configured into the application (maxGetGames).
  *
  * @param loginEvidence Client id and token evidence of of the logged-in user.
  * @param fromEpochSecond Inclusive low end of the time range, as an epoch second.
  * @param toEpochSecond Exclusive high end of the time range, as an epoch second.
  * @param maxGames - Bound on the number of games to retrieve.
  */
case class GetUserGamesRequest(
  loginEvidence: AuthEvidence,
  fromEpochSecond: Long,
  toEpochSecond: Long,
  maxGames: Int
)
