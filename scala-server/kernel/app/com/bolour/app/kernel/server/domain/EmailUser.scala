/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.domain

import com.bolour.util.CommonUtil.ID

case class EmailUser(id: ID, email: String, nickname: String)

object EmailConstants {
  val guestEmail: String = "guest@noguestemail.com"
}

