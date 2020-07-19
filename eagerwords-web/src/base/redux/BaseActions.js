/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {baseActionTypes} from "./BaseActionTypes";

// TODO. Obsolete.
export function authChanged(authenticated, userInfo) {
  return {
    type: baseActionTypes.authChanged,
    authenticated,
    userInfo
  };
}

// TODO. Obsolete.
export function loggedInAsGuest() {
  return {
    type: baseActionTypes.loggedInAsGuest
  }
}

// TODO. Obsolete.
export function loggedOut() {
  return {
    type: baseActionTypes.loggedOut
  }
}

export function serverInfo(serverType, apiVersion) {
  return {
    type: baseActionTypes.serverInfo,
    serverType,
    apiVersion,
  }
}
