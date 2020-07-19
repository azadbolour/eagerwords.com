/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.app.kernel.server.domain

import com.bolour.util.CommonUtil.{Email, ID}

/**
  * User.
  *
  * @param id The unique identifier of the user in the application db.
  * @param userId The unique identifier of the user as a user
  *                       in the authentication provider's system.
  * @param name The name of the user. For maximum security we just use the first name.
  * @param email The user's email.
  *              The email is the unique "natural key" of the user in the application database.
  * @param licenseAccepted The user has accepted the terms of service.
  */
case class User(id: ID, userId: String, name: String, email: Email, licenseAccepted: Boolean)

object UserObject {
  val unAssignedId = ""
}
