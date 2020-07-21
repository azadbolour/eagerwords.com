/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import PropTypes from 'prop-types';

import React, { Component } from 'react';
import { connect } from 'react-redux';
import { push } from 'connected-react-router'
import {createStructuredSelector} from 'reselect'

import Button from "react-bootstrap/Button";
import {stringify} from "../../base/util/Logger";
import TextInput from "../../base/components/TextInput";
import {enteredAsGuest} from "../redux/AuthActions";
import {EulaComponent} from './EulaComponent';
import {selectLoggedOut, selectNickname} from '../redux/AuthSelector';
import 'bootstrap/dist/css/bootstrap.min.css';
import {
  defaultOpContext,
  opMockEffects, serviceStateSettingInterceptor,
} from "../../base/components/ComponentProcessingStateManager";

import {
  ServiceProcessingDecorator,
} from "../../base/components/ComponentProcessingStateManager";
import {authService, initLoginDisplay} from "../service/AuthService";
import {resultMapValue} from "../../base/domain/Result";
import BareTextInput from "../../base/components/BareTextInput";
import validator from "validator";
import {testingControlName} from "../../base/components/Testing2Components";

export const gotoSignUpButtonTestId = 'signUpButton';
export const signInButtonTestId = 'signInButton';
export const emailTestId = 'emailInput';

const validMessage = null;
const invalidEmailMessage = "not a valid email address";
const emailRequiredMessage = "*";

/**
 * An application's landing components. The top level components
 * that a random user sees coming into the application for te first
 * time. Presents the user with a choice of signing up, logging on,
 * or entering as guest.
 *
 * A generic components not dependent on a particular application.
 */
class LandingComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      opContext: defaultOpContext,
      showGuestEula: false,
      email: null,
      invalidEmailMessage: validMessage,

    };
    this.authService = authService;
  }

  setEmailState = (email) => {
    this.setState((state, props) => {
      return {...state, email}
    })
  };

  // TODO. Duplicated in SignUpStartComponent. Move to BareTextInput.

  setEmailValidity = (email) => {
    let isEmailValid = validator.isEmail(email);
    let message = (isEmailValid) ? validMessage : invalidEmailMessage;
    this.setState((state, props) => {
      return {...state, invalidEmailMessage: message}
    });
  };

  RequestLogin = (props) => {
    let login = () => this.login(this.state.email);
    let setEmail = this.setEmailState;
    let message = this.state.invalidEmailMessage;
    if (this.state.email === null)
        message = emailRequiredMessage;
    let disabled = message !== validMessage;

    let onEmailChange = (email) => {
      this.setEmailState(email);
      this.setEmailValidity(email);
    };

    return (
      <div style={{display: 'flex', flexDirection: 'column'}}>
        <BareTextInput name='email' placeholder='sign-in email'
          data-testid={emailTestId}
          onChange={ev => onEmailChange(ev.target.value)}
          message={message}
        />
        <Button className='mt-1' size="lg" variant="success" style={{width: "150px"}}
          data-testid={signInButtonTestId}
          onClick={login}
          disabled={disabled}
        >
          Sign In
        </Button>
      </div>
    )
  };
  // <Button className='mx-3' size="lg" variant="success" style={{width: "100px"}}

  RequestSignUp = (props) => {
    let gotoSignUp = this.gotoSignUp;
    return (
      <div>
        <Button data-testid={gotoSignUpButtonTestId} className='mr-3' style={{width: '140px'}}
          size="md" variant="success" onClick={() => gotoSignUp()}>Sign Up</Button>
      </div>
    )
  };

  RequestGuestEntry = (props) => {
    let enterAsGuest = this.enterAsGuest;
    return (
      <div>
        <Button className='mr-3' size="md" variant="success" style={{width: '140px'}}
                onClick={() => enterAsGuest()}>Enter as Guest</Button>
      </div>
    )
  };

  GuestEulaModal = (props) => {
    let show = this.state.showGuestEula;
    let EulaTextComponent = this.props.EulaTextComponent;

    console.log(`GuestEulaModal - show: ${show}`);

    let closer = () => this.props.enterAsGuest();

    let canceller = () => this.setState((state) => {
        return {...state, showGuestEula: false}
      });

    console.log(`rendering EulaComponent`);
    return (
      <EulaComponent show={show} closer={closer} canceller={canceller} EulaTextComponent={EulaTextComponent}/>
    )
  };

  render = () => {
    const testingControls= [
      testingControlName.unrecoverable,
      testingControlName.timeout
    ];

    // In prod build, <this.Comp> is undefined inside the JSX. Not in dev mode!
    let it = this;

    return (
      <div>
      <it.GuestEulaModal/>
      <ServiceProcessingDecorator
        comp={it}
        testingControls={testingControls}
        errorCallback={() => this.props.done(null)}
      >
        <div style={{display: 'flex', flexDirection: 'row'}}>
          <it.RequestSignUp/>
          <br/>
          <it.RequestGuestEntry/>
          <br/>
        </div>
        <br/>
        <br/>
        <it.RequestLogin/>
        <br/>
      </ServiceProcessingDecorator>
    </div>
    )
  }
  ;

  enterAsGuest = () => {
    this.setState((state, props) => {
      return {...state, showGuestEula: true}
    });
    // this.props.enterAsGuest();
  };

  gotoSignUp = () => this.props.showSignUp();

  // TODO. Change promise to vow in names in functions.
  // TODO. URGENT. Increase timeout and move to an app settings module.
  loginTimeoutSecs = 60; // Duration that login stays valid.
  loginWaitTimeoutMillis = 2000;

  login = (email) => {
    // let effects = {mockEffects: opMockEffects(this)};
    let mockEffects = opMockEffects(this);
    let resultPromise = serviceStateSettingInterceptor(this, initLoginDisplay,
      this.authService, this.authService.initLogin, email, this.loginTimeoutSecs, mockEffects);
    resultPromise.then(resultMapValue((data) => this.props.done(data.clientId)));
  }
}


export function mapStateToProps(store) {
  return createStructuredSelector({
    loggedOut: selectLoggedOut,
    nickname: selectNickname
  })(store);
}

export function mapDispatchToProps(dispatch) {
  return {
    enterAsGuest: () => {
        dispatch(enteredAsGuest());
        dispatch(push('/play'));
      },
    showSignUp: () => { dispatch(push('/signup')) },
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(LandingComponent);
