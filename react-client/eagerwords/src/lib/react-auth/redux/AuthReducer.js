/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import produce from 'immer';
import {authActionTypes} from "./AuthActionTypes";
import {guestName} from "../Constants";

export const authInitialState = {
  clientId: null,
  token: null,
  nickname: null,
  isGuest: false
};

export const authReducer = (state = authInitialState, action) => produce(state, draft => {
  switch (action.type) {
    case authActionTypes.enteredAsGuest:
      draft.clientId = null;
      draft.token = null;
      draft.nickname = guestName;
      draft.isGuest = true;
      break;
    case authActionTypes.loginConfirmed:
      draft.clientId = action.clientId;
      draft.token = action.token;
      draft.nickname = action.nickname;
      draft.isGuest = false;
      break;
    case authActionTypes.signUpConfirmed:
      draft.clientId = action.clientId;
      draft.token = action.token;
      draft.nickname = action.nickname;
      draft.isGuest = false;
      break;
    case authActionTypes.loggedOut:
      draft.clientId = null;
      draft.token = null;
      draft.nickname = null;
      draft.isGuest = false;
      break;
  }
});
