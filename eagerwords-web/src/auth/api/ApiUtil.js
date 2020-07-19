/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import AuthClientApi from "./AuthClientApi";
import AuthMockApi from "./AuthMockApi";

import {apiTypes, getApiType, getServerUrl} from '../../envvars';
import {mkTimeoutProxy} from "../../base/util/MiscUtil";

// export const gameApis = {
//     [apiTypes.mock]: GameMockApi,
//     [apiTypes.client]: GameClientApi
// };

// export const apiImpl = function(apis) {
//   let apiType = getApiType();
//   let serverUrl = getServerUrl();
//   return apis[apiType];
// };

// TODO. Use env var.
//  1 minute should be sufficient
const timeoutMillis = 1000 * 60;

const mkClientApi = function() {
  let api = new AuthClientApi(getServerUrl());
  let apiWithTimeout = mkTimeoutProxy(api, timeoutMillis);
  return apiWithTimeout;
};

export const authApis = {
    [apiTypes.mock]: AuthMockApi, // TODO. new?? proxy?
    [apiTypes.client]: mkClientApi()
};

