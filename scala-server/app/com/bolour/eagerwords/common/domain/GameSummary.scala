/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.domain

/**
  * Summary of game reported to clients after the last play.
  *
  * @param stopInfo Data about why the last play was reached.
  */
case class GameSummary(stopInfo: StopInfo)
