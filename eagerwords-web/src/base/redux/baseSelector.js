/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import {createSelector} from 'reselect';

function selectSlice(state, props) {
  return state.baseReducer;
}

export const selectServerInfo = _ => createSelector (
  selectSlice,
  (slice) => slice.serverInfo
);