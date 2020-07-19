/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import {mkUser} from "../domain/User";

export const UserConverter = {
  toJson(user) {
    return {
      userId: user.userId,
      name: user.name,
      email: user.email,
      licenseAccepted: user.licenseAccepted
    }
  },
  fromJson(dto) {
    return mkUser(
      dto.userId,
      dto.name,
      dto.email,
      dto.licenseAccepted
    )
  }
};



