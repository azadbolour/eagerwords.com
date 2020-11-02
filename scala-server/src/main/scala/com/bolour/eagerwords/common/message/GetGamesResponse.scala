/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.message

/**
  * API response to a request to get basic information about a set of games.
  */
case class GetGamesResponse(
  games: List[GameBasicInfo]
)
