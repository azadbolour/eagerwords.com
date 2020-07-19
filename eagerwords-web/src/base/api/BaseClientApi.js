/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/** @module GeneralClientApi */

import {restManager} from '../util/RestManager';
import {stringify} from "../util/Logger";

class BaseClientApi {
  constructor(serverUrl) {
    this.serverUrl = serverUrl;
  }

  // TODO. Duplicated in Auth sub-system. Make sure the same as auth version and remove from Auth.
  handShake() {
    let request = restManager.mkGetRequest();
    let promise = restManager.send(request, this.serverUrl, '/base/hand-shake');
    return promise;
  }

  // @Deprecated. TODO. Remove. Not required for passwordless authentication.
  saveUser(jsonUser) {
    let body = JSON.stringify(jsonUser);
    let request = restManager.mkPostRequest(body);
    let promise = restManager.send(request, this.serverUrl, '/base/user');
    return promise;
  }

  /**
   * Return promise of a response structure whose json is the userDto.
   * Non-existent user is not an error - it is returned with ok status with a
   * json of null. Consistent with Option of None being represented as json null.
   */
  // @Deprecated. TODO. Remove. Not required for passwordless authentication.
  getUser(userId) {
    let request = restManager.mkGetRequest();
    let restPath = `/base/user/${userId}`;
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }
}

export default BaseClientApi;
