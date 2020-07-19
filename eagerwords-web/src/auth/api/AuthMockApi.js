/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {happyResponsePromise, unHappyResponsePromise} from "../../base/util/HttpUtil";
import {stringify} from "../../base/util/Logger";
import {errorTags} from "../util/AuthErrors";

// TODO. Keep track of login timeout and return true/false accordingly.
// TODO. Update tests using this API.

const nowSecs = () => Math.floor(Date.now()/1000);

const authenticationFailedJson = {
  tag: errorTags.missingAuthEvidence,
  message: "invalid authentication evidence"
};

class AuthMockApi {
  constructor() {
    this.email = null;
    this.nickname = null;
    this.clientId = "12345678";
    this.loginExpiration = 0;
  }

  handShake = () => {
    let json = {
      serverType: "Mock",
      apiVersion: "1.0"
    };
    return happyResponsePromise(json);
  };

  // TODO. Keep track of nickname and use it for confirmLogin.

  initSignUp = (email, nickname) => {
    this.email = email;
    this.nickname = nickname;
    let json = {clientId: this.clientId};
    return happyResponsePromise(json);
  };

  confirmSignUp = (clientId, token) => {
    if (clientId !== this.clientId) {
      return unHappyResponsePromise(authenticationFailedJson, 422);
    }
    return happyResponsePromise({});
  };

  initLogin = (email, loginTimeout) => {
    if (email !== this.email) {
      let json = {
        tag: errorTags.notSignedUp,
        message: `unknown email ${email} - please sign up first`
      };
      return unHappyResponsePromise(json, 422);
    }

    this.loginExpiration = nowSecs + loginTimeout;
    return happyResponsePromise({clientId: this.clientId});
  };

  confirmLogin = (clientId, token) => {
    if (clientId !== this.clientId) {
      return unHappyResponsePromise(authenticationFailedJson, 422);
    }
    return happyResponsePromise({nickname: this.nickname});
  };

  logout = (clientId, token) => {
    if (clientId !== this.clientId) {
      return unHappyResponsePromise(authenticationFailedJson, 422);
    }
    this.clientId = null;
    this.loginExpiration = 0;
    return happyResponsePromise({nickname: this.nickname});
  };

  isLoggedIn = (loginEvidence) => {
    if (!loginEvidence)
      return false;
    if (loginEvidence.clientId !== this.clientId) {
      return unHappyResponsePromise(authenticationFailedJson, 422);
    }
    return Promise.resolve(nowSecs < this.loginExpiration);
  };
}

export default AuthMockApi;











