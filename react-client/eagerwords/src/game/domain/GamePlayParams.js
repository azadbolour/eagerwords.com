/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

// import {coinToss} from "../../jsutil/util/MiscUtil";
import {MiscUtil} from 'lib/js-util/index';

let {coinToss} = MiscUtil;
export const validDimensions = [5, 7, 9, 11, 13, 15, 17];
export const defaultDimension = 11;

export const validTrayCapacity = [5, 6, 7, 8, 9];
export const defaultTrayCapacity = 7;

export const defaultLanguageCode = 'en';

export const playerTypes = {
  userPlayer: 'User',
  machinePlayer: 'Machine'
};

export const randomStartingPlayer = function() {
  const {userPlayer, machinePlayer} = playerTypes;
  return coinToss(userPlayer, machinePlayer);
};

export const fixStartingPlayer = function(startingPlayer) {
  return (startingPlayer !== randomPlayerType) ? startingPlayer : randomStartingPlayer();
};

export const randomPlayerType = 'Random';

export const pieceProviderTypes = {
  random: "Random",
  cyclic: "Cyclic"
};

export const defaultPieceProviderType = pieceProviderTypes.random;

export const defaultGamePlayParams = {
  dimension: defaultDimension,
  trayCapacity: defaultTrayCapacity,
  languageCode: defaultLanguageCode,
  pieceProviderType: defaultPieceProviderType,
  startingPlayer: randomPlayerType
};