/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/** @module BaseClientApi */

import {RestManager} from 'lib/js-util/index';
import {stringify} from 'lib/js-util/index';

let {restManager} = RestManager;

// TODO. Logging for requests and responses.

class AuthClientApi {
  constructor(serverUrl) {
    this.serverUrl = serverUrl;
  }

  callPostForUser(data, actionName) {
    let body = JSON.stringify(data);
    let request = restManager.mkPostRequest(body);
    let restPath = `/auth/${actionName.toLowerCase()}`;
    console.log(`callPostForUser: ${this.serverUrl} ${restPath}`);
    // console.log(`callPostForUser - body: ${body}`);
    let promise = restManager.send(request, this.serverUrl, restPath);
    let promise1 = promise.then(response => {
      console.log(`response: ${stringify(response)}`);
      return response;
    });
    return promise1;
  }

  initSignUp(email, nickname) {
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
}

export default AuthClientApi;
