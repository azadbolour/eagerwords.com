/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {coinToss, getOrElseF, reverseObject, warn} from "../../base/util/MiscUtil";
import {deviceTypes} from "../../base/domain/DeviceTypes";

// export const playerTypes = {
//   userPlayer: 'User',
//   machinePlayer: 'Machine'
// };
// // export const defaultStartingPlayer = playerTypes.machinePlayer;
//
// export const randomPlayerType = 'Random';
//
// export const pieceProviderTypes = {
//   random: "Random",
//   cyclic: "Cyclic"
// };
//
// export const defaultPieceProviderType = pieceProviderTypes.random;
// export const defaultPreferredDevice = deviceTypes.mouse;
//
// export const defaultDimension = 11;
// export const defaultSquarePixels = 40; // No more.
// export const defaultTrayCapacity = 7;

// export const defaultGameSettings = {
//   dimension: defaultDimension,
//   squarePixels: defaultSquarePixels,
//   trayCapacity: defaultTrayCapacity,
//   languageCode: 'en',
//   pieceProviderType: defaultPieceProviderType,
//   startingPlayer: randomPlayerType,
//   preferredDevice: null  // This is an option. None is encoded as null.
// };

// export const randomStartingPlayer = function() {
//   const {userPlayer, machinePlayer} = playerTypes;
//   return coinToss(userPlayer, machinePlayer);
// };

// export const fixStartingPlayer = function(settings) {
//   return (settings.startingPlayer !== randomPlayerType) ?
//     settings :
//     {...settings, startingPlayer: randomStartingPlayer()}
// };

// export const validDimensions = [5, 7, 9, 11, 13, 15, 17];
// export const validSquareSizes = ['small', 'normal', 'large'];
// export const validTrayCapacity = [5, 6, 7, 8, 9];

/* Square size, value font and piece font are all indexed by 'small', 'normal', and 'large'. Only
 * the square size object needs a reversed version because its pixel value is stored on the server
 * and needs to be reverse mapped to a string for the settings form.
 */

// export const squareSizeToPixels = {
//   small: 25,
//   normal: 40,
//   large: 55
// };

// export const squarePixelsToSize = reverseObject(squareSizeToPixels); // No more.

// No more.
// export const safeSquarePixelsToSize = (pixels) =>
//   getOrElseF(squarePixelsToSize, pixels, () => {
//     warn("selectGameSquareSize: can't convert pixel size:", pixels);
//     return 'normal';
//   });

// export const valueFont = {
//   small: 7,
//   normal: 9,
//   large: 11
// };
//
// export const pieceFont = {
//   small: 12,
//   normal: 22,
//   large: 24
// };
