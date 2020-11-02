/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

// import {getServerUrl} from "../../envvars";
import GameClientApi from "./GameClientApi";
// import {mkTimeoutProxy} from "../../jsutil/util/MiscUtil";
import {stringify} from 'lib/js-util/index';
import {MiscUtil} from 'lib/js-util/index';
import {envvars} from 'lib/js-util/index';

let {getServerUrl} = envvars;
let {mkTimeoutProxy} = MiscUtil;

// TODO. Use an env var.
const timeoutMillis = 1000 * 60;

export const mkClientApi = function() {
  let api = new GameClientApi(getServerUrl());
  let apiWithTimeout = mkTimeoutProxy(api, timeoutMillis);
  return apiWithTimeout;
};
const serverUrl = getServerUrl();

