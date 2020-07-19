/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {mkErrorState, toCamelCase} from "./MiscUtil";
import {stringify} from "./Logger";

/** @module UrlUtil */

export const queryParams = function(location) {
  /**
   * Get query parameters from the url.
   * @param location - The url is called location in javascript.
   * @returns A javascript object with query parameter names as field names,
   * and an array of the corresponding query parameter values as a field's value.
   */
  let getQueryParams = function(location) {
    let queryParams = {};
    location.search.substr(1).split('&').forEach(function(item) {
      let keyValue = item.split('='),
        key = keyValue[0],
        value = keyValue[1] && decodeURIComponent(keyValue[1]);
      (key in queryParams) ? queryParams[key].push(value) : queryParams[key] = [value]
    });
    return queryParams;
  };

  let _params = getQueryParams(location);

  return {
    /**
     * Get the first or the only value of a query parameter.
     * @param name The name of the query parameter.
     * @returns The value or 'undefined' if there is no query parameter with the given name.
     */
    getParam: function(name) {
      return (name in _params) ? _params[name][0] : undefined;
    }
  };
};

/**
 * Validate and extract a set of query parameters.
 * Parameter name parts are dash-separated. Corresponding fields of the extracted object are camelCase.
 *
 * @param queryParams The query parameter object obtained from the URL.
 * @param paramSpec Include the names and types of the parameters to be extracted.
 * @param validator The parameter validator.
 *
 * @returns {extracted, errorState} The extracted object and validation errors.
 */
export const queryParamsToObject = function(queryParams, paramSpec, validator) {
  let errorState = mkErrorState();
  let extracted = {};

  for (let name in paramSpec) {
    if (!paramSpec.hasOwnProperty(name))
      continue;
    let value = queryParams.getParam(name);
    if (value === undefined)
      continue;
    if (paramSpec[name] === 'int') {
      // TODO. Abstract and bullet-proof integer parsing to a util method.
      value = (/[0-9]+/).test ? Number(value) : NaN;
      if (isNaN(value)) {
        let message = `invalid value ${value} for numeric parameter ${name}`;
        errorState.addError(message);
        continue;
      }
    }
    let {valid, message} = validator(name, value);
    if (!valid) {
      errorState.addError(message);
      continue;
    }
    let fieldName = toCamelCase(name);
    extracted[fieldName] = value;
  }

  return {
    extracted,
    errorState
  };

};


