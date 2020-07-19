/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React, { Component } from 'react';
import { connect } from 'react-redux';
import {
  defaultOpContext,
  opMockEffects, serviceStateSettingInterceptor,
} from "../../base/components/ComponentProcessingStateManager";
import TextInput from "../../base/components/TextInput";
import Button from "react-bootstrap/Button";
import {createStructuredSelector} from "reselect";
import {Header} from "semantic-ui-react";
import {
  ServiceProcessingDecorator,
} from "../../base/components/ComponentProcessingStateManager";
import {authService, confirmLoginDisplay} from "../service/AuthService";
import {resultMapValue} from "../../base/domain/Result";
import BareTextInput from "../../base/components/BareTextInput";
import {testingControlName} from "../../base/components/Testing2Components";

export const confirmSignInButtonTestId = 'confirmSignInButton';
export const tokenTestId = 'tokenInput';

// TODO. URGENT. Provide a button for did not get email - go back to sign in.

/**
 * A sub-components of the landing components that allows the user to confirm
 * his ownership of the email with which he is trying to log in. The initial
 * login step sends the user's email to the backend server. The backend server
 * in turn sends a confirmation token to the user's email to make sure the
 * user controls the email. The user is then prompted by this components
 * to confirm his login by providing the confirmation token to complete
 * the login process.
 */
class SignInConfirmComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      opContext: defaultOpContext,
      token: null,
    };
    this.authService = authService;
  }

  setTokenState = (token) => {
    console.log(`setTokenState - token: ${token}`);
    this.setState((state, props) => {
      return {...state, token}
    })
  };

  render = () => {
    let confirmLogin = () => this.confirmLogin(this.state.token);
    let setToken = this.setTokenState;
    const testingControls= [
      testingControlName.unrecoverable,
      testingControlName.timeout
    ];

    return (
      <div>
        <div>
          <Header as="h4" textAlign="center" style={{color: 'DarkGoldenRod'}}>Sign In Confirmation</Header>
        </div>
        <ServiceProcessingDecorator
          comp={this}
          testingControls={testingControls}
          errorCallback={() => this.props.done(null, null)}
        >
          <br />
          <label>
            To complete your sign-in, please check your email for your sign-in token, enter it below, and <em>Confirm Sign In</em>.
          </label>
          <br /><br />
          <div style={{display: 'flex', flexDirection: 'row'}}>
            <BareTextInput
              data-testid={tokenTestId}
              name='token' placeholder='token'
              onChange={ev => setToken(ev.target.value)}
            />
            <Button
              data-testid={confirmSignInButtonTestId}
              className='mx-3' size="lg" variant="success"
              onClick={confirmLogin}
            >
              Confirm Sign In
            </Button>
          </div>
        </ServiceProcessingDecorator>
      </div>
    )
  };

  // loginTimeoutMillis = 2000;

  confirmLogin(token) {
    let clientId = this.props.clientId;
    // let effects = {mockEffects: opMockEffects(this)};
    let mockEffects = opMockEffects(this);
    let resultPromise = serviceStateSettingInterceptor(this, confirmLoginDisplay,
      this.authService, this.authService.confirmLogin, clientId, token, mockEffects);
    resultPromise.then(resultMapValue((data) => this.props.done(token, data.nickname)));
  }
}

export function mapStateToProps(store) {
  return createStructuredSelector({
  })(store);
}

export function mapDispatchToProps(dispatch) {
  return {
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(SignInConfirmComponent);