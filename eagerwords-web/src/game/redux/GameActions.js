/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {gameActionTypes} from "./GameActionTypes";
import {stringify} from "../../base/util/Logger";

export function gameSelectedForResumption(gameId) {
  return {
    type: gameActionTypes.gameSelectedForResumption,
    gameId
  };
}


