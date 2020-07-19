/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React from "react";

// This leads to error: 'each child must have unique key'. TODO. Fix later.
export const spacer = function(spaces) {
  if (spaces === undefined)
    spaces = 2;
  let arr = [];
  for (let i = 0; i < spaces; i++)
    arr.push(<span>&nbsp;</span>);
  return arr;
};