/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import BaseService from "./BaseService";
import {errorText} from '../util/HttpUtil';
import {OK} from './ServiceHelper';
import {error} from '../../base/util/MiscUtil';

// @alleycat
// temporary fix for being unable to save the Eula flag in the DB.
// set to true to exit the infinite loop.
const FORCE_EULA_OK = true;
// demonstrate error handling.
const MOCK_GET_USER_ERROR = false;

export const mkUserHandler = function(appConfig) {
  const _service = new BaseService(appConfig);

  return {
    saveUser: function(user) {
      return _service.saveUser(user).then( response => {
        const ok = response.ok;
        const message = ok ? OK : errorText(response);
        return {ok, message};
      }).catch((err) => {
        error('UserHandler saveEula()', err);
        return Promise.reject(err);
      })
    },

    /**
     * Get a user by user id.
     * @param userId The unique external identifier of the user.
     * @returns {ok, result} ok is a boolean indicating normal result
     *          result is a user for ok and an error message for !ok
     */
    getUser: function(userId) {
      return _service.getUser(userId).then( response => {
        if (MOCK_GET_USER_ERROR)
          return Promise.reject("[mock error] couldn't get user " + userId);
        const ok = response.ok;
        const result = ok ? response.json : errorText(response);
        return {ok, result};
      }).catch((err) => {
        error('UserHandler getUser()', err);
        return Promise.reject(err);
      })
    },

    checkEula: function(userId, onNeedEula=null) {
      return this.getUser(userId)
        .then(handle)
        .catch((err) => {
          error('UserHandler checkEula()', err);
          return Promise.reject(err);
        });

      function handle (response) {
        if (FORCE_EULA_OK) return true;

        const {ok, result} = response;
        if (!ok) {
          return Promise.reject('Unable to get user for id ' + userId);
        }
        const user = result;
        if (user === null) {
          return Promise.reject('Unable to get user for id ' + userId + ' (result was null)');
        }
        if (!user.licenseAccepted) {
          if (onNeedEula) onNeedEula();
          return false;
        }
        return true;
      }
    }
  };
};
