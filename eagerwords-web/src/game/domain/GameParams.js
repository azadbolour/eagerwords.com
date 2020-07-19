import {playerTypes} from "./GameSettings";

export const machineStarts = gameParams => gameParams.startingPlayer === playerTypes.machinePlayer;
export const playerStarts = gameParams => !machineStarts(gameParams);
