/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {MiscUtil} from 'lib/js-util/index';
import {envvars} from 'lib/js-util/index';
import AuthClientApi from "./AuthClientApi";

let {getServerUrl} = envvars;
let {mkTimeoutProxy} = MiscUtil;

//  1 minute should be sufficient
const timeoutMillis = 1000 * 60;

export const mkClientApi = function() {
  let api = new AuthClientApi(getServerUrl());
  let apiWithTimeout = mkTimeoutProxy(api, timeoutMillis);
  return apiWithTimeout;
};


