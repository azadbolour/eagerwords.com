import {playerTypes} from "./GamePlayParams";

export const machineStarts = gameParams => gameParams.playParams.startingPlayer === playerTypes.machinePlayer;
export const playerStarts = gameParams => !machineStarts(gameParams);

export const mkGameParams = function(playParams, pointValues) {
  return {
    playParams,
    pointValues
  }
};
