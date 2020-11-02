/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.message

import com.bolour.auth.common.domain.AuthEvidence
import com.bolour.eagerwords.common.domain.UserGameSettings

case class SaveUserGameSettingsRequest(
  loginEvidence: AuthEvidence,
  settings: UserGameSettings
)
