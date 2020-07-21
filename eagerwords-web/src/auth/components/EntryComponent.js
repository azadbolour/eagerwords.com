/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { Redirect } from "react-router-dom";
import { connect } from 'react-redux';
import {createStructuredSelector} from 'reselect'
import { Header } from 'semantic-ui-react';
import {selectLoggedOut, selectLoginEvidence} from '../redux/AuthSelector';
import 'bootstrap/dist/css/bootstrap.min.css';
import {
  defaultOpContext,
  resetOpMessages,
  opProcessingMsg,
  opErrorMsg, serviceStateSettingInterceptor,
} from "../../base/components/ComponentProcessingStateManager";
import LandingComponent from "./LandingComponent";
import SignInConfirmComponent from "./SignInConfirmComponent";
import {loginConfirmed} from "../redux/AuthActions";
import {
  ServiceProcessingDecorator
} from "../../base/components/ComponentProcessingStateManager";
import {authService, isLoggedInDisplay} from "../service/AuthService";
import {stringify} from "../../base/util/Logger";
import {resultMapValue} from "../../base/domain/Result";
import {GenericFooter} from "../../base/components/GenericFooter";

// TODO. Skeletal framework should be visible in all modes.
// TODO. A logged in user should not be able to enter as guest. Make sure.

const modes = {
  start: "start",
  confirm: "confirm",
  home: "home"
};

/**
 * The highest level entry point to an application.
 *
 * A generic components. Its dependencies on a particular application
 * are provided as props.
 *
 * If the browser session includes login evidence and that evidence
 * is checked to be current, this components causes the application's
 * 'Home' components to be rendered.
 *
 * Otherwise, the landing components is rendered for the user to login,
 * sign up, or enter as guest.
 */
class EntryComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      opContext: defaultOpContext,
      mode: null,
      clientId: null
    };
    this.authService = authService;
  }

  // TODO. Consistent naming of component props.

  static propTypes = {
    homePath: PropTypes.string.isRequired,
    header: PropTypes.string.isRequired,
    EulaTextComponent: PropTypes.func.isRequired,
    PrivacyComponent: PropTypes.func.isRequired,
    NoticesComponent: PropTypes.func.isRequired,
    AboutComponent: PropTypes.func.isRequired,
  };

  componentDidMount = () => {
    console.log("entry components componentDidMount");

    let definitelyLoggedOut = this.props.loggedOut;
    /*
     * The user's login may have expired on the server - while
     * loginEvidence is still valid in the client.
     *
     * But that has to be checked in the home component anyway.
     * So unless we know definitely that the user is logged out,
     * just move on to the home component.
     */
    let mode = definitelyLoggedOut ? modes.start : modes.home;
    console.log(`setting mode to: ${stringify(mode)}`);
    this.setModeState(mode);

    // let resolution = (loggedIn) => {
    //   let mode = loggedIn ? modes.home : modes.start;
    //   console.log(`setting mode to: ${stringify(mode)}`);
    //   this.setModeState(mode);
    // };
    // let resultPromise = serviceStateSettingInterceptor(this, isLoggedInDisplay, this.authService,
    //   this.authService.isLoggedIn, this.props.loginEvidence);
    // resultPromise.then(resultMapValue(resolution));
  };

  validLoginEvidence() {
    const evidence = this.props.loginEvidence;
    if (!evidence)
      return false;
    const {clientId, token} = evidence;
    return clientId !== null && token !== null;
  }

  componentDidUpdate() {
    // If a logout has occurred while on the home page, go back to start of the login process.
    let currentMode = this.state.mode;
    if (currentMode !== modes.home)
      return;

    let stillLoggedIn = this.validLoginEvidence();
    if (!stillLoggedIn)
      this.setModeState(modes.start);
  }

  setModeState(mode) {
    console.log(`setting mode to: ${stringify(mode)}`);
    this.setState((state, props) => { return {...state, mode} });
  }

  setClientIdState(clientId) {
    this.setState((state, props) => { return {...state, clientId} });
  }

  render = () => {
    let header = this.props.header;
    let homePath = this.props.homePath;
    let {EulaTextComponent, PrivacyComponent, NoticesComponent, AboutComponent} = this.props;

    let processingMessage = opProcessingMsg(this);
    let processing = processingMessage != null;
    let unrecoverableMessage = opErrorMsg(this);
    let unrecoverable = unrecoverableMessage !== null;

    console.log(`processing: ${stringify(processing)}, unrecoverable: ${stringify(unrecoverable)}`);

    let modeEnabled = !processing && !unrecoverable;

    let mode = this.state.mode;
    console.log(`landing component render - mode: ${mode}, modeEnabled: ${modeEnabled}`);

    let showStart = modeEnabled && mode === modes.start;
    let showConfirmed = modeEnabled && mode === modes.confirm;
    let showHome = modeEnabled && mode === modes.home;
    let clientId = this.state.clientId;
    let it = this;

    console.log(`showStart: ${stringify(showStart)}`);
    console.log(`showConfirmed: ${stringify(showConfirmed)}`);
    console.log(`showHome: ${stringify(showHome)}`);

    let loginRequested = (clientId) => {
      if (clientId === null) {// There was an issue with the login request - so start over.
        resetOpMessages(this);
        this.setModeState(modes.start);
      }
      else {
        this.setClientIdState(clientId);
        this.setModeState(modes.confirm);
      }
    };

    let loginConfirmed = (token, nickname) => {
      console.log(`loginConfirmed - nickname: ${nickname}`);
      if (token === null) {
        resetOpMessages(this);
        this.setModeState(modes.start);
      }
      else {
        let clientId = this.state.clientId;
        this.props.loginConfirmed(clientId, token, nickname);
        this.setModeState(modes.home);
      }
    };

    let Footer = () => {return (<GenericFooter
      EulaTextComponent={EulaTextComponent}
      PrivacyComponent={PrivacyComponent}
      NoticesComponent={NoticesComponent}
      AboutComponent={AboutComponent}
      />)};

    return (
      <div>
        <div>
          {!showHome && <Header as="h3" textAlign="center" style={{color: 'DarkGoldenRod'}}>{header}</Header>}
        </div>

        <ServiceProcessingDecorator comp={it}>
          <div>
            <div>
              {showStart && <div>
                <LandingComponent
                  EulaTextComponent={EulaTextComponent}
                  done={(clientId) => loginRequested(clientId)}/></div>}
              {showConfirmed && <div><SignInConfirmComponent clientId={clientId} done={(token, nickname) => loginConfirmed(token, nickname)}/></div>}
            </div>
            {!showHome && <Footer/>}
          </div>
          {showHome && <Redirect to={homePath}/>}
        </ServiceProcessingDecorator>
      </div>
    )
  };
}

export function mapStateToProps(store) {
  return createStructuredSelector({
    loggedOut: selectLoggedOut,
    loginEvidence: selectLoginEvidence,
  })(store);
}

export function mapDispatchToProps(dispatch) {
  return {
    loginConfirmed: (clientId, token, nickname) => {
      dispatch(loginConfirmed(clientId, token, nickname));
    },

  }
}

export default connect(mapStateToProps, mapDispatchToProps)(EntryComponent);
