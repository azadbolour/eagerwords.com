/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {stringify} from "../util/Logger";

/**
 * For now we use the default error message provided by the server.
 * Future clients may wish to further customize messages using specific
 * fields of each type of error. But note that as of March 2020 the error
 * API has not been standardized.
 */
export const errorText = (response) => {
  let json = response.json;
  let message = json.message;
  return (message !== undefined) ? message : ((json !== undefined) ? stringify(json) : '');
};

export const happyResponsePromise = function(json) {
  return new Promise(function(resolve, reject) {
    resolve({
      ok: true,
      json: json,
      status: 200,
      statusText: "OK"
    });
  });
};

export const unHappyResponsePromise = function(json, status) {
  return new Promise(function(resolve, reject) {
    resolve({
      ok: false,
      json: json,
      status: status,
      statusText: `error ${status}`
    });
  });
};

/**
 * Return a duplicate response object whose json is replaced by a
 * corresponding domain object.
 */
export const convertHappyResponse = function(response, domainObj) {
  if (!response.ok)
    return response;

  let newResponse = Object.create(response);
  newResponse.json = domainObj;
  return newResponse;
};
