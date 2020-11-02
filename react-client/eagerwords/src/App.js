/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React, { Component } from 'react';
import { Route } from 'react-router';
import { ConnectedRouter } from 'connected-react-router';
import { Container } from 'semantic-ui-react';
import {EntryComponent} from 'lib/react-auth/index';
import GamesComponent from './game/components/GamesComponent';
// import ToastComponent from './lib/react-support/index/components/ToastComponent';
import {ToastComponent} from 'lib/react-support/index';
import {EulaPrefaceComponent} from "./game/components/informational/EulaPrefaceComponent";
import SettingsComponent from './game/components/SettingsComponent';
import PlayComponent from './game/components/PlayComponent';
// import ErrorBoundary from './lib/react-support/index/components/ErrorBoundaryComponent';
import {ErrorBoundaryComponent} from 'lib/react-support/index';
import {SignUpComponent} from 'lib/react-auth/index';
import Modal from 'react-modal';
import 'react-toastify/dist/ReactToastify.min.css';
import history from './history';
import {EulaTextComponent} from './game/components/informational/EulaTextComponent';
import {PrivacyComponent} from './game/components/informational/PrivacyComponent';
import {AboutComponent} from './game/components/informational/AboutComponent';
import {authRoutingPaths} from "lib/react-auth/index";
import {gameRoutingPaths} from "./game/components/GameRoutingPaths";

// @alleycat This is recommended for accessibility, see docs.
Modal.setAppElement('#root');

const entryHeader = "Welcome to Eager Words";
const signUpHeader = "Eager Words Sign Up";

// TODO. Constants for router paths.

let gotoEntry = () => <EntryComponent
  homePath={gameRoutingPaths.games}
  header={entryHeader}
  EulaPrefaceComponent={EulaPrefaceComponent}
  EulaTextComponent={EulaTextComponent}
  PrivacyComponent={PrivacyComponent}
  AboutComponent={AboutComponent}
/>;

let gotoSignUp = () => <SignUpComponent
  homePath={gameRoutingPaths.games}
  header={signUpHeader}
  EulaPrefaceComponent={EulaPrefaceComponent}
  EulaTextComponent={EulaTextComponent}
  PrivacyComponent={PrivacyComponent}
  AboutComponent={AboutComponent}
/>;

class App extends Component {
  render() {
    return (
      <ConnectedRouter history={history}>
        <Container text style={{ marginTop: '1em' }}>
          <ErrorBoundaryComponent>
            <ToastComponent/>
            <Route path={authRoutingPaths.entry} exact render={gotoEntry}/>
            <Route path={authRoutingPaths.signUp} render={gotoSignUp} />
            <Route path={gameRoutingPaths.games} component={GamesComponent} />
            <Route path={gameRoutingPaths.play} component={PlayComponent} />
            <Route path={gameRoutingPaths.settings} component={SettingsComponent} />
          </ErrorBoundaryComponent>
        </Container>
      </ConnectedRouter>
    );
  }
}

export default App;

