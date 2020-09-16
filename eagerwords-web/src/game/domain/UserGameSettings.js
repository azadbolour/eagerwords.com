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