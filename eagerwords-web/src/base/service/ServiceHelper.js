/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

export const OK = "OK";
export const opSuccess = true;
export const opFailure = false;

export const successful = function(opData) {
  return opData.ok;
};

export const failed = function(opData) {
  return !opData.ok;
};
