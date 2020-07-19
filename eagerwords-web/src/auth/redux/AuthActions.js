/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {authActionTypes} from "./AuthActionTypes";
import {baseActionTypes} from "../../base/redux/BaseActionTypes";

export function loginConfirmed(clientId, token, nickname) {
  return {
    type: authActionTypes.loginConfirmed,
    clientId,
    nickname,
    token
  };
}

export function signUpConfirmed(clientId, token, nickname) {
  console.log(`signUpConfirmed - clientId: ${clientId}, token: ${token}`);
  return {
    type: authActionTypes.signUpConfirmed,
    clientId,
    token,
    nickname
  };
}

export function enteredAsGuest() {
  return {
    type: authActionTypes.enteredAsGuest
  };
}

export function loggedOut() {
  return {
    type: authActionTypes.loggedOut
  }
}

