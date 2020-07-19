/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

package com.bolour.eagerwords.common.message

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.eagerwords.common.domain.GameSettings

case class SaveUserGameSettingsRequest(
  loginEvidence: AuthEvidence,
  settings: GameSettings
)
