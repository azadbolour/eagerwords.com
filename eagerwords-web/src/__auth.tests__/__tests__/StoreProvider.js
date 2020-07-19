/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React from 'react'
import {Provider} from "react-redux";
import history from "../history";
import configureStore from "../configureStore";

export const StoreProvider = (props) => {
  let preLoadedState = props.preLoadedState;
  let store = configureStore(preLoadedState, history);
  return (
    <Provider store={store}>
      {props.children}
    </Provider>
  );
};



