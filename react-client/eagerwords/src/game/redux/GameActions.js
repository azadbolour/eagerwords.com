/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {gameActionTypes} from "./GameActionTypes";
import {stringify} from 'lib/js-util/index';

export function serverInfo(serverType, apiVersion) {
  return {
    type: gameActionTypes.serverInfo,
    serverType,
    apiVersion,
  }
}

export function gameSelectedForResumption(gameId) {
  return {
    type: gameActionTypes.gameSelectedForResumption,
    gameId
  };
}


