/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {defaultGamePlayParams} from "./GamePlayParams";
import {defaultGameLookAndFeelParams} from "./GameLookAndFeelParams";

export const mkUserGameSettings = function(playSettings, lookAndFeelSettings) {
  return {
    playSettings,
    lookAndFeelSettings
  }
};

export const defaultUserGameSettings =
  mkUserGameSettings(defaultGamePlayParams, defaultGameLookAndFeelParams);