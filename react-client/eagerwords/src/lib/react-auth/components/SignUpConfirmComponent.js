/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React, { Component } from 'react';
import { connect } from 'react-redux';
import {ComponentOperationState} from 'lib/react-support/index';
import Button from "react-bootstrap/Button";
import {createStructuredSelector} from "reselect";
import {Header} from "semantic-ui-react";
import {BareTextInput} from "lib/react-support/index";
import {TestingComponents} from 'lib/react-support/index';
import {ResultModule} from "lib/js-util/index";
import {ComponentProcessingStateManager} from 'lib/react-support/index';
import {stringify} from "../../js-util";
import {opErrorTag} from "../../react-support/components/ComponentOperationState";
import {authService, confirmSignUpDisplay} from "../service/AuthService";
import {errorTags} from "../util/AuthErrors";

let {
  serviceStateSettingInterceptor,
  ServiceProcessingDecorator
} = ComponentProcessingStateManager;
let {testingControlName} = TestingComponents;
let {defaultOpContext, opMockEffects} = ComponentOperationState;
let {resultMapValue} = ResultModule;

export const confirmSignUpButtonTestId = 'confirmSignUpButton';
export const tokenTestId = 'tokenInput';

const maxTries = 3;

// TODO. URGENT. Provide a button for did not get email - go back to sign up.

class SignUpConfirmComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      opContext: defaultOpContext,
      token: null,
      confirmationTries: 0,
    };
    this.authService = authService;
  }

  setTokenState = (token) => {
    this.setState((state, props) => {
      return {...state, token}
    })
  };

  incrementConfirmationTriesState = () => {
    let confirmationTries = this.state.confirmationTries + 1;
    this.setState((state, props) => {
      return {...state, confirmationTries}
    })
  };

  /**
   * Allow a number of tries to get the token right.
   */
  confirmationErrorCallback = () => {
    let isTokenMismatch = opErrorTag(this) === errorTags.confirmationTokenMismatch;
    let tries = this.state.confirmationTries;
    if (!isTokenMismatch || tries > maxTries)
      this.props.done(null);
  };

  render = () => {
    let confirmSignUp = () => this.confirmSignUp(this.state.token);
    let setToken = this.setTokenState;
    const testingControls= [
      testingControlName.unrecoverable,
      testingControlName.timeout
    ];
    let it = this;

    return (
      <div>
        <div>
          <Header as="h4" textAlign="center" style={{color: 'DarkGoldenRod'}}>Sign Up Confirmation</Header>
        </div>

        <ServiceProcessingDecorator
          comp={it}
          testingControls={testingControls}
          errorCallback={() => it.confirmationErrorCallback()}
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
    let mockEffects = opMockEffects(this);
    this.incrementConfirmationTriesState();
    let resultPromise = serviceStateSettingInterceptor(this, confirmSignUpDisplay,
      this.authService, this.authService.confirmSignUp, clientId, token, mockEffects);
    resultPromise.then(result => {
      // console.log(`confirm login - result: ${stringify(result)}`);
      /*
       * Error results are processed in confirmation error callback.
       * We may get here from an initial token mismatch error.
       * They keep us in this component to allow the token to be corrected.
       * Only signal success if the result if OK.
       */
      if (result.isValue)
        this.props.done(token, result.data.nickname)
    });
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
