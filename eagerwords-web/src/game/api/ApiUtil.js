/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import {apiTypes, getServerUrl} from "../../envvars";
import GameMockApi from "./GameMockApi";
import GameClientApi from "./GameClientApi";
import {mkTimeoutProxy} from "../../base/util/MiscUtil";

// TODO. Use an env var.
const timeoutMillis = 1000 * 60;

const mkClientApi = function() {
  let api = new GameClientApi(getServerUrl());
  let apiWithTimeout = mkTimeoutProxy(api, timeoutMillis);
  return apiWithTimeout;
};
const serverUrl = getServerUrl();

export const gameApis = {
  [apiTypes.mock]: GameMockApi,
  [apiTypes.client]: new GameClientApi(serverUrl)
};
