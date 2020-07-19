/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/** @module BaseClientApi */

import {restManager} from '../../base/util/RestManager';
import {stringify} from "../../base/util/Logger";
import {timedPromise} from "../../base/util/MiscUtil";

// TODO. Logging for requests and responses.

class AuthClientApi {
  constructor(serverUrl) {
    this.serverUrl = serverUrl;
  }

  callPostForUser(data, actionName) {
    let body = JSON.stringify(data);
    let request = restManager.mkPostRequest(body);
    let restPath = `/auth/${actionName.toLowerCase()}`;
    console.log(`callPostForUser - serverUrl: ${this.serverUrl}`);
    console.log(`callPostForUser - restPath: ${restPath}`);
    console.log(`callPostForUser - body: ${body}`);
    let promise = restManager.send(request, this.serverUrl, restPath);
    let promise1 = promise.then(response => {
      console.log(`response: ${stringify(response)}`);
      return response;
    });
    return promise1;
  }

  initSignUp(email, nickname) {
    console.log(`initSignUp - nickname: ${nickname}`);
    let data = {email, nickname};
    return this.callPostForUser(data, 'initSignUp');
  }

  confirmSignUp(clientId, token) {
    let authEvidence = {clientId, token};
    return this.callPostForUser(authEvidence, 'confirmSignUp');
  }

  initLogin(email, loginTimeout) {
    let data = {email, loginTimeout};
    return this.callPostForUser(data, 'initLogin');
  }

  confirmLogin(clientId, token) {
    let authEvidence = {clientId, token};
    return this.callPostForUser(authEvidence, 'confirmLogin');
  }

  logout(clientId, token) {
    let authEvidence = {clientId, token};
    return this.callPostForUser(authEvidence, 'logout');
  }

  isLoggedIn(loginEvidence) {
    return this.callPostForUser(loginEvidence, 'loggedIn');
  }

  // TODO. Will be obsoleted.
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
  // TODO. Will be obsoleted.
  getUser(userId) {
    let request = restManager.mkGetRequest();
    let restPath = `/base/user/${userId}`;
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }
}

export default AuthClientApi;
