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
import {authService, confirmSignUpDisplay} from "../service/AuthService";
import {resultMapValue} from "../../base/domain/Result";
import BareTextInput from "../../base/components/BareTextInput";
import {testingControlName} from '../../base/components/Testing2Components';

export const confirmSignUpButtonTestId = 'confirmSignUpButton';
export const tokenTestId = 'tokenInput';

// TODO. URGENT. Provide a button for did not get email - go back to sign up.

class SignUpConfirmComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      opContext: defaultOpContext,
      token: null,
    };
    this.authService = authService;
  }

  setTokenState = (token) => {
    this.setState((state, props) => {
      return {...state, token}
    })
  };

  render = () => {
    let confirmSignUp = () => this.confirmSignUp(this.state.token);
    let setToken = this.setTokenState;
    const testingControls= [
      testingControlName.unrecoverable,
      testingControlName.timeout
    ];

    return (
      <div>
        <div>
          <Header as="h4" textAlign="center" style={{color: 'DarkGoldenRod'}}>Sign Up Confirmation</Header>
        </div>

        <ServiceProcessingDecorator
          comp={this}
          testingControls={testingControls}
          errorCallback={() => this.props.done(null)}
        >
          <br />
          <label>
            To complete your sign-up, please check your email for your sign-up token, enter it below, and <em>Confirm Sign Up</em>.
          </label>
          <br /><br />
          <div style={{display: 'flex', flexDirection: 'row'}}>
            <BareTextInput
              data-testid={tokenTestId}
              name='token' placeholder='token'
              onChange={ev => setToken(ev.target.value)}
            />
            <Button
              data-testid={confirmSignUpButtonTestId}
              className='mx-3' size="lg" variant="success"
              onClick={confirmSignUp}>
              Confirm Sign Up
            </Button>
          </div>
        </ServiceProcessingDecorator>
      </div>
    )
  };

  confirmSignUp = (token) => {
    let clientId = this.props.clientId;
    // let effects = {mockEffects: opMockEffects(this)};
    let mockEffects = opMockEffects(this);
    let resultPromise = serviceStateSettingInterceptor(this, confirmSignUpDisplay,
      this.authService, this.authService.confirmSignUp, clientId, token, mockEffects);
    resultPromise.then(resultMapValue(() => this.props.done(token)));
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

export default connect(mapStateToProps, mapDispatchToProps)(SignUpConfirmComponent);