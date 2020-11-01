/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { Redirect } from "react-router-dom";
import {
  defaultOpContext,
  defaultOpMessages, opErrorMsg, opProcessingMsg,
  resetOpMessages, serviceStateSettingInterceptor,
  setOpProcessingMsg,
} from "../../base/components/ComponentProcessingStateManager";
import {createStructuredSelector} from "reselect";
import {selectLoggedOut, selectLoginEvidence} from "../redux/AuthSelector";
import {connect} from "react-redux";
import {signUpConfirmed} from "../redux/AuthActions";
import {Header} from "semantic-ui-react";
import {ProcessingSpinner, UnrecoverableModal} from "../../base/components/NotificationComponents";
import SignUpStartComponent from "./SignUpStartComponent";
import SignUpConfirmComponent from "./SignUpConfirmComponent";
import {authService, isLoggedInDisplay} from "../service/AuthService";
import {stringify} from "../../base/util/Logger";
import {resultMapValue} from "../../base/domain/Result";
import {GenericFooter} from "../../base/components/GenericFooter";

const modes = {
  start: "start",
  confirm: "confirm",
  home: "home"
};

// export const signUpHeading = "Eager Words Sign Up";

/**
 * High-level components for doing initial sign-up. A generic component.
 * Its dependencies on a particular application are provided in props.
 *
 * This component has two sub-components, SignUpStartComponent and SignUpConfirmComponent.
 * Sign-up is a 2-step process including initialization and confirmation, and the
 * renderer of this components renders the corresponding sub-components for each step.
 *
 * The sub-components themselves are not individually accessible directly, making it easy
 * to prevent subversion of the protocol for the 2 steps.
 *
 * The contract for the sub-components is that they get a callback function called 'done'
 * as a property, and the done callback will return the information needed by this component
 * from the particular step. A null value returned by 'done' for a required piece of information
 * from a sub-components indicates an error.
 *
 * A similar pattern is used for other multi-step actions, like sign-in.
 */
class SignUpComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      opContext: defaultOpContext,
      mode: null,
      clientId: null,
      nickname: null
    };
    this.authService = authService;
  }

  static propTypes = {
    homePath: PropTypes.string.isRequired,
    header: PropTypes.string.isRequired,
    EulaTextComponent: PropTypes.func.isRequired
  };

  componentDidMount = async () => {
    console.log("signup components componentDidMount");

    let definitelyLoggedOut = this.props.loggedOut;
    /*
     * A logged-in user does not need to sign-up.
     * To sign up as another user, the user has to first logout.
     *
     * The user's login may have expired on the server - while
     * loginEvidence is still valid in the client.
     *
     * But that has to be checked in the home component anyway.
     * So unless we know definitely that the user is logged out,
     * just move on to the home component.
     */
    let mode = definitelyLoggedOut ? modes.start : modes.home;
    this.setModeState(mode);

    // let action = () => this.authService.isLoggedIn(this.props.loginEvidence);
    // let resolution = (loggedIn) => {
    //   let mode = loggedIn ? modes.home : modes.start;
    //   this.setModeState(mode);
    // };
    // // let resultPromise = serviceStateSettingInterceptorLegacy(this, isLoggedInDisplay, action);
    // let resultPromise = serviceStateSettingInterceptor(this, isLoggedInDisplay,
    //   this.authService, this.authService.isLoggedIn, this.props.loginEvidence);
    // resultPromise.then(resultMapValue(resolution));
  };

  setModeState(mode) {
    this.setState((state, props) => { return {...state, mode} });
  }

  setClientIdState(clientId) {
    this.setState((state, props) => { return {...state, clientId} });
  }

  setNicknameState(nickname) {
    this.setState((state, props) => { return {...state, nickname} });
  }

  render = () => {
    let header = this.props.header;
    let processingMessage = opProcessingMsg(this);
    let processing = processingMessage != null;
    let unrecoverableMessage = opErrorMsg(this);
    let unrecoverable = unrecoverableMessage !== null;
    let clientId = this.state.clientId;

    let modeEnabled = !processing && !unrecoverable;

    let mode = this.state.mode;
    console.log(`sign up component render - mode: ${mode}, modeEnabled: ${modeEnabled}`);

    let showStart = modeEnabled && mode === modes.start;
    let showConfirmed = modeEnabled && mode === modes.confirm;
    let showHome = modeEnabled && mode === modes.home;
    let homePath = this.props.homePath;

    let {EulaTextComponent, PrivacyComponent, AboutComponent} = this.props;

    let Footer = () => {return (<GenericFooter
      EulaTextComponent={EulaTextComponent}
      PrivacyComponent={PrivacyComponent}
      AboutComponent={AboutComponent}
    />)};

    let signUpRequested = (clientId, nickname) => {
      console.log(`clientId returned from signUpStart: ${clientId}`);
      if (clientId === null) { // Indicates error.
        resetOpMessages(this);
        this.setModeState(modes.start);
      }
      else {
        this.setClientIdState(clientId);
        this.setNicknameState(nickname);
        this.setModeState(modes.confirm);
        console.log(`state for happy path: ${stringify(this.state)}`);
      }
    };

    let signUpConfirmed = (token) => {
      console.log(`signupConfirmed - nickname: ${this.state.nickname}`);
      if (token === null) {
        resetOpMessages(this);
        this.setModeState(modes.start);
      }
      else {
        let clientId = this.state.clientId;
        let nickname = this.state.nickname;
        this.props.signUpConfirmed(clientId, token, nickname);
        this.setModeState(modes.home);
        console.log(`state for happy path: ${stringify(this.state)}`);
      }
    };

    return (
      <div>
        <div>
          <Header as="h3" textAlign="center" style={{color: 'DarkGoldenRod'}}>{header}</Header>
        </div>

        {unrecoverable && <div><UnrecoverableModal message={unrecoverableMessage}/></div>}

        <div>
          {processing && <div><ProcessingSpinner message={processingMessage}/></div>}
        </div>

        <div>
          <div>
            <div>
            {showStart &&
              <div><SignUpStartComponent
                EulaTextComponent={EulaTextComponent}
                done={(clientId, nickname) => signUpRequested(clientId, nickname)}/>
              </div>
            }
            {showConfirmed &&
              <div><SignUpConfirmComponent
                clientId={clientId}
                done={(token) => signUpConfirmed(token)}/>
              </div>
            }
            </div>
            {!showHome && <Footer/>}
          </div>
          {showHome && <Redirect to={homePath}/>}
        </div>
      </div>
    )
  }
}

export function mapStateToProps(store) {
  return createStructuredSelector({
    loggedOut: selectLoggedOut,
    loginEvidence: selectLoginEvidence
  })(store);
}

export function mapDispatchToProps(dispatch) {
  return {
    signUpConfirmed: (clientId, token, nickname) => {
      dispatch(signUpConfirmed(clientId, token, nickname));
    },
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(SignUpComponent);

