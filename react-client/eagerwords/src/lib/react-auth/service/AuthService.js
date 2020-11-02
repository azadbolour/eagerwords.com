/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {stringify} from 'lib/js-util/index';
import {ApiAdapters} from 'lib/js-util/index';
import {ResultModule, VowModule} from "lib/js-util/index";
import {mkClientApi} from "../api/ApiUtil";
import {authResponseToResultPromiseMapper} from '../util/AuthErrors';

let {valueResult} = ResultModule;
let {Vow} = VowModule;

let {apiMockErrorAdapter, defaultMockOpEffects} = ApiAdapters;

// TODO. Test mocking of errors etc. as the code has changed.

const serviceWrapper = function(description, mockEffects, api, func, ...args) {
  // let mockErrors = safeGetOrElse(effects, 'mockEffects', defaultMockOpEffects);
  let promise = apiMockErrorAdapter(description, mockEffects)(api, func, ...args);
  let promise1 = promise.then(response => {
    console.log(`response: ${stringify(response)}`);
    return response;
  });
  return Vow(authResponseToResultPromiseMapper(promise1));
};

export const initSignUpDisplay = 'initializing sign up';
export const confirmSignUpDisplay = 'confirming sign up';
export const initLoginDisplay = 'initializing login';
export const confirmLoginDisplay = 'confirming login';
export const logoutDisplay = 'logout';
export const isLoggedInDisplay = 'checking login';

export const authService = (function() {
  let api = mkClientApi();

  return {
    initSignUp: (email, nickname, mockEffects = defaultMockOpEffects) => {
      console.log(`authService.initSignUp - email: ${email}`);
      return serviceWrapper(initSignUpDisplay, mockEffects, api, api.initSignUp, email, nickname);
    },

    confirmSignUp: (clientId, token, mockEffects = defaultMockOpEffects) => {
      return serviceWrapper(confirmSignUpDisplay, mockEffects, api, api.confirmSignUp, clientId, token);
    },

    initLogin: (email, loginTimeout, mockEffects = defaultMockOpEffects) => {
      return serviceWrapper(initLoginDisplay, mockEffects, api, api.initLogin, email, loginTimeout);
    },

    confirmLogin: (clientId, token, mockEffects = defaultMockOpEffects) => {
      return serviceWrapper(confirmLoginDisplay, mockEffects, api, api.confirmLogin, clientId, token);
    },

    logout: (clientId, token, mockEffects = defaultMockOpEffects) => {
      return serviceWrapper(logoutDisplay, mockEffects, api, api.logout, clientId, token);
    },

    isLoggedIn: (loginEvidence, mockEffects = defaultMockOpEffects) => {
      if (noLoginEvidence(loginEvidence))
        return Promise.resolve(valueResult(false));
      return serviceWrapper(isLoggedInDisplay, mockEffects, api, api.isLoggedIn, loginEvidence);
    },
  }
})();

const noLoginEvidence = (ev) => !ev || !ev.clientId || !ev.token;
