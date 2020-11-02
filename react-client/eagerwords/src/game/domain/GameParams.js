import {playerTypes} from "./GamePlayParams";

export const machineStarts = gameParams => gameParams.playParams.startingPlayer === playerTypes.machinePlayer;
export const playerStarts = gameParams => !machineStarts(gameParams);

/**
 * Create game parameters.
 *
 * @param playParams Parameters governing play (independent of UI).
 * @param pointValues Values associated with the board squares.
 */
export const mkGameParams = function(playParams, pointValues) {
  return {
    playParams,
    pointValues
  }
};
