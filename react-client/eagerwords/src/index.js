/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import ReactDOM from 'react-dom';
import {Provider} from 'react-redux';
import './index.css';
import App from './App';
import * as serviceWorker from './serviceWorker';
import configureStore from './configureStore'
// import {stringify, stringifyNoBracesForEmpty} from './jsutil/util/Logger';
import {Logger} from 'lib/js-util/index';
import {gameInitialState} from "./game/redux/GameReducer";
import {authInitialState} from "lib/react-auth/index";
import history from './history';
import {HttpUtil} from "lib/js-util/index";
import {serverInfo} from "./game/redux/GameActions";
import {mkClientApi} from "./game/api/ApiUtil";

let {errorText} = HttpUtil;
let {stringify, stringifyNoBracesForEmpty} = Logger;

const preLoadedState = {
  authReducer: authInitialState,
  gameReducer: {...gameInitialState},
};

const store = configureStore(preLoadedState, history);

async function doHandShake(dispatch) {
  const api = mkClientApi();
  let serverType = "unknown";
  let apiVersion = "unknown";

  await api.handShake().then(response => {
    if (!response.ok)
      console.error(`server handshake failed - ${stringify(errorText(response))}`);
    else {
      serverType = response.json.serverType;
      apiVersion = response.json.apiVersion;
      dispatch(serverInfo(serverType, apiVersion));
    }
  }).catch(reason => {
      console.error(`server handshake failed - ${stringifyNoBracesForEmpty(reason)}`);
  })
}

doHandShake(store.dispatch);

// const hostname = document.location.hostname;

const appElement = document.getElementById('root');
const reduxApp =
  <Provider store={store}>
    <App />
  </Provider>;

ReactDOM.render(reduxApp, appElement);

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
