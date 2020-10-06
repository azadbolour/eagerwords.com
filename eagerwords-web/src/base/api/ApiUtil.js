/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {apiTypes} from '../../envvars';
import BaseClientApi from "./BaseClientApi";
import BaseMockApi from "./BaseMockApi";
import {getServerUrl} from "../../envvars";
import {mkTimeoutProxy} from "../util/MiscUtil";
import {stringify} from "../util/Logger";

// TODO. Use an env var.
const timeoutMillis = 1000 * 60;

const mkClientApi = function() {
  let serverUrl = getServerUrl();
  console.log(`mkClientApi: serverUrl: ${stringify(serverUrl)}`);
  let api = new BaseClientApi(serverUrl);
  let apiWithTimeout = mkTimeoutProxy(api, timeoutMillis);
  return apiWithTimeout;
};

export const baseApis = {
    [apiTypes.mock]: BaseMockApi, // TODO. new?? proxy??
    [apiTypes.client]: mkClientApi()
};
