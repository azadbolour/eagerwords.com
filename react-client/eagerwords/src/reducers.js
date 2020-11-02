/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import { combineReducers } from 'redux'
import { connectRouter } from 'connected-react-router'
import gameReducer from './game/redux/GameReducer'
// import {authReducer} from './auth/redux/AuthReducer'
import {authReducer} from 'lib/react-auth/index';

const createRootReducer = (history) => combineReducers({
  router: connectRouter(history),
  authReducer,
  gameReducer
});

export default createRootReducer
