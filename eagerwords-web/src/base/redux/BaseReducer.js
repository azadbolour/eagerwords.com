/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import produce from "immer";
import {baseActionTypes} from "./BaseActionTypes";
import {stringify} from "../util/Logger";

export const baseInitialState = {
  serverInfo: {
    serverType: "unknown",
    apiVersion: "unknown"
  }
};

export const baseReducer = (state = baseInitialState, action) =>
  produce(state, draft => {
    switch (action.type) {
      case baseActionTypes.serverInfo:
        draft.serverInfo = {
          serverType: action.serverType,
          apiVersion: action.apiVersion
        };
        break;
      default:
        break;
    }
});
