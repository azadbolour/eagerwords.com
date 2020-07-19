/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */
package com.bolour.eagerwords.common.message

import com.bolour.util.CommonUtil.{Email, ID}

/**
  * Data transfer object for getting minimal information about games.
  */
case class GameBasicInfo(
  gameId: ID,
  // userId: String,
  firstSecond: Long,
  lastSecond: Long,
  status: String,
  userScore: Int,
  machineScore: Int
)
