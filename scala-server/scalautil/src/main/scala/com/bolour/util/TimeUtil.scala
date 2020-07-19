/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *    https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.util

import java.time.Instant

object TimeUtil {
  def nowSecs: Long = Instant.now().getEpochSecond
}
