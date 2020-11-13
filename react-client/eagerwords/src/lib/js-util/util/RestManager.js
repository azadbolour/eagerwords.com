/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/**
 * Utility functions for making rest calls.
 */
import {stringify, stringifyNoBracesForEmpty} from "./Logger";

/**
 * Process the response and return the json happy result or
 * the json error in a json property.
 *
 * For normal errors, the response will be unprocessable entity (status 422)
 * and the message will be in the json body.
 *
 * For errors returned by the server-side HTTP framework, just log the body
 * (since we are not sure it will be meaningful to the user), and return
 * the response's errorText as the error message.
 *
 */
const processResponse = function(response) {
  // console.log(`response: ${stringify(response)}`);
  let resp = {
    ok: response.ok,
    status: response.status,
    statusText: response.statusText
  };
  if (response.ok || response.status === 422) {
    return response.json().then(json => {return {...resp, json}})
  }
  else {
    let body = (typeof(response.body) === 'function') ? response.body : ( () => Promise.resolve({}));
    return body().then(body => {
      let bodyText = stringifyNoBracesForEmpty(body);
      // Log the body if any. It may not be meaningful as a user message.
      console.log(`error response: ${stringify({...resp, bodyText})}`);
      return {...resp, json: {message: resp.statusText}};
    })
  }
};

/**
 * Extend status code to cover rejections.
 */
export const fetchRejectStatusCode = 550;

const rejectMessage = (reason) => {
  return "The application encountered a (possibly temporary) network disconnect trying to communicate "
  + "with the server over the internet." + ` Internal message: '${reason}.'`
};

export const restManager = {
  /**
   * Send a message to the server and process the response into the following
   * data structure:
   *
   *    {
   *      ok         // True/false for happy/error response.
   *      status     // The HTTP status, e.g., 404.
   *      statusText // The text of the HTTP status, e.g. 'Not Found'
   *      json       // The response json. In case of error - will have a message property.
   *    }
   *
   * @param request The HTTP request - includes verb abd body.
   * @param serverUrl The base URL of the server.
   * @param restPath The path in the URL.
   *
   */
  send(request, serverUrl, restPath) {
    let restUrl = serverUrl + restPath;
    console.log(`send by fetch - restPath: ${stringify(restPath)}`);
    return fetch(restUrl, request).then(response => {
      return processResponse(response)
    }).catch(reason => {
      console.log(`restManager - rejected fetch: ${reason}`);
      let message = rejectMessage(reason);
      return {
        ok: false,
        status: fetchRejectStatusCode,
        statusText: 'Rejected',
        json: {message}
      };
    });
  },

  headers() {
    return {
      'Accept': 'application/json',
      'Content-Type': 'application/json',
      mode: 'same-origin' ,
    };
  },

  headersForEmptyBody() {
    return {
      'Accept': 'application/json',
      'Content-Type': 'application/json',
      mode: 'same-origin',
      'Content-Length': '0' // ,
    };
  },

  mkGetRequest() {
    return {
      method: 'GET',
      headers: this.headers()
    };
  },

  mkPostRequest(body) {
    console.log(`body: ${body}`);
    return {
      method: 'POST',
      headers: this.headers(),
      body: body
    };
  },

  mkEmptyPostRequest() {
    return {
      method: 'POST',
      headers: this.headersForEmptyBody()
    };
  }
};
