/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/**
 * @module GeneralService
 */

import {baseApis} from '../api/ApiUtil';
import {UserConverter} from "../api/BaseConverters";
import {convertHappyResponse} from "../util/HttpUtil";
import {stringify} from "../util/Logger";
import {getApiType} from "../../envvars";

// TODO. Refactor to the latest service pattern using Vow and Result.
// See authService.

class BaseService {
  constructor() {
    this.api = baseApis[getApiType()];
  }

  handShake() {
    // Handshake response is {serverType, apiVersion} - so no need for happy conversion.
    return this.api.handShake();
  }

  // TODO. Obsolete. Remove.
  saveUser(user) {
    let jsonUser = UserConverter.toJson(user);
    // Response data structure is unit. So no happy conversion needed.
    return this.api.saveUser(jsonUser);
  }

  /**
   * Get user with a given user id.
   * Returns a user object if the userId exists, null if it does not,
   * in a response structure, whose ok value indicates success or failure.
   * Non-existence is NOT considered failure.
   */
  // TODO. Obsolete. Remove.
  getUser(userId) {
    let promise = this.api.getUser(userId);
    return promise.then(response => {
      if (!response.ok)
        return response;
      let userDto = response.json;
      let user = userDto === null ? null : UserConverter.fromJson(userDto);
      return convertHappyResponse(response, user);
    });
  }
}

export default BaseService;
