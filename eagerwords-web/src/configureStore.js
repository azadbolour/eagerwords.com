/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import { applyMiddleware, compose, createStore } from 'redux'
import { routerMiddleware } from 'connected-react-router'
import persistState from 'redux-localstorage'
import createRootReducer from './reducers'

export default function configureStore(preloadedState, history) {
  const store = createStore(
    createRootReducer(history), // root reducer with router state
    preloadedState,
    compose(
      applyMiddleware(
        routerMiddleware(history), // for dispatching history actions
        // ... other middlewares ...
      ),
      // No path: persist the entire store.
      // No config: use the storage key 'redux'.
      persistState(/*paths, config*/),
    ),
  );
  return store
}
