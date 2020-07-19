/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {createSelector} from 'reselect';
import {stringify} from "../../base/util/Logger";

const selectAuth = state => state.authReducer;

export const selectNickname = createSelector(selectAuth, s => s.nickname);

export const selectLoginEvidence =
  createSelector(selectAuth, st => {
    console.log(`selecting from ${stringify(st)}`);

    return st.isGuest ? null : {
      clientId: st.clientId,
      token: st.token
    };
  });

export const selectLoggedOut = createSelector(
  selectLoginEvidence,
  ev => !ev || ev.clientId === null || ev.token === null
);

export const selectIsGuest =
  createSelector(selectAuth, st => st.isGuest);
