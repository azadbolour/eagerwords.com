/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import produce from 'immer';
import {gameActionTypes} from "./GameActionTypes";

// TODO. Separate out the different slices of the redux state.

/*
 The userGameSettings state is initialized to {} before it is retrieved from the database.
 If the database does not contain a settings for the current user, userGameSettings
 becomes null. Otherwise it gets the settings stored in the database for the user.
 And it is kept consistent with changes made to settings by the user.
 */

export const gameInitialState = {
  serverInfo: {serverType: 'unknown', apiVersion: 'unknown'},
  gameIdToResume: null,
};

/* eslint-disable default-case, no-param-reassign */
const gameReducer = (state = gameInitialState, action) =>
  produce(state, draft => {
    switch (action.type) {
      case gameActionTypes.serverInfo:
        draft.serverInfo = {
          serverType: action.serverType,
          apiVersion: action.apiVersion
        };
        break;
      case gameActionTypes.gameSelectedForResumption:
        draft.gameIdToResume = action.gameId;
        break;
    }
  });

export default gameReducer;
