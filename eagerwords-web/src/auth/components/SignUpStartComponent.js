/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { connect } from 'react-redux';
import {createStructuredSelector} from 'reselect'
import validator from 'validator';

import Button from "react-bootstrap/Button";
import {stringify} from "../../base/util/Logger";
import TextInput from "../../base/components/TextInput";
import 'bootstrap/dist/css/bootstrap.min.css';
import {
  defaultOpContext,
  opMockEffects, serviceStateSettingInterceptor,
} from "../../base/components/ComponentProcessingStateManager";
import {
  ServiceProcessingDecorator,
} from "../../base/components/ComponentProcessingStateManager";
import {authService, initSignUpDisplay} from "../service/AuthService";
import {resultMapValue} from "../../base/domain/Result";
import {InvalidInputModal} from "../../base/components/NotificationComponents";
import BareTextInput from "../../base/components/BareTextInput";
import {EulaComponent} from "./EulaComponent";
import {testingControlName} from "../../base/components/Testing2Components";

export const signUpButtonTestId = 'signUpButton';
export const emailTestId = 'emailInput';
export const nicknameTestId = 'nicknameInput';

const invalidEmailMessage = "not a valid email address";
const emailRequiredMessage = "email required";
const invalidNicknameMessage = "not a valid nickname";
const nicknameRequiredMessage = "nickname required ";
const validNickname = " [alphanumeric]";
const validMessage = null;

class SignUpStartComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      opContext: defaultOpContext,
      email: null,
      nickname: null,
      showEula: false,
      showInvalidModal: false,
      invalidEmailMessage: validMessage,
      invalidNicknameMessage: validMessage
    };
    this.authService = authService;
  }

  static propTypes = {
    EulaTextComponent: PropTypes.func.isRequired
  };

  setEmailState = (email) => {
    this.setState((state, props) => {
      return {...state, email, invalidEmailMessage: validMessage}
    });
  };

  setEmailValidity = (email) => {
    let isEmailValid = validator.isEmail(email);
    let message = (isEmailValid) ? validMessage : invalidEmailMessage;
    this.setState((state, props) => {
      return {...state, invalidEmailMessage: message}
    });
  };

  checkEmailValid = async () => {
    let valid = this.state.invalidEmailMessage === validMessage;
    if (this.state.email === null) {
      valid = false;
      await this.setState((state, props) => {
        return {...state, invalidEmailMessage: emailRequiredMessage}
      });
    }
    return valid;
  };

  setNicknameState = (nickname) => {
    this.setState((state) => {
      return {...state, nickname, invalidNicknameMessage: validMessage}
    });
  };

  setNicknameValidity = (nickname) => {
    let isNicknameValid = !validator.isEmpty(nickname) && validator.isAlphanumeric(nickname);
    let message = (isNicknameValid) ? validMessage : invalidNicknameMessage;
    this.setState((state) => {
      return {...state, invalidNicknameMessage: message}
    });
  };

  checkNicknameValid = async () => {
    let valid = this.state.invalidNicknameMessage === validMessage;
    if (this.state.nickname === null) {
      valid = false;
      await this.setState((state, props) => {
        return {...state, invalidNicknameMessage: nicknameRequiredMessage}
      });
    }
    return valid;
  };

  InvalidsModal = () => {
    let show = this.state.showInvalidModal;
    let emailMessage = this.state.invalidEmailMessage;
    let nicknameMessage = this.state.invalidNicknameMessage;
    let closer = () => {
      this.setState((state) => {return {...state, showInvalidModal: false}})
    };
    return (
      <div>
        <InvalidInputModal show={show} closer={closer}>
          <div>
            <div>{emailMessage}</div>
            <div>{nicknameMessage}</div>
          </div>
        </InvalidInputModal>
      </div>
    )
  };

  EulaModal = (props) => {
    let show = this.state.showEula;
    let EulaTextComponent = this.props.EulaTextComponent;

    // TODO. State vars can be obtained in signup.
    let closer = () => this.signUp(this.state.email, this.state.nickname);

    let canceller = () => this.setState((state) => {
      return {...state, showEula: false}
    });

    return (
      <EulaComponent show={show} closer={closer} canceller={canceller} EulaTextComponent={EulaTextComponent}/>
    )
  };

  RequestSignUp = (props) => {
    // let signUp = () => this.signUp(this.state.email, this.state.nickname, this.state.eulaAccepted);

    let startSignUp = async () => {
      // let valid = this.checkEmailValid() && this.checkNicknameValid();

      let validEmail = await this.checkEmailValid();
      let validNickname = await this.checkNicknameValid();

      let valid = validEmail && validNickname;
      let state = this.state;

      console.log(`state: ${stringify(state)}`);

      let showEula = valid;
      let showInvalidModal = !valid;
      this.setState((state) => {
        return {...state, showEula, showInvalidModal}
      });
    };

    let setEmail = this.setEmailState;
    let setNickname = this.setNicknameState;

    // TODO. Formatting.
    // TODO. Use sub-components for each widget to help readability.

    let invalidEmailMessage = this.state.invalidEmailMessage;
    let invalidNicknameMessage = this.state.invalidNicknameMessage;

    return (
      <div>
        <this.InvalidsModal/>
        <this.EulaModal/>
      <div>
        <br />
        <BareTextInput
          data-testid={emailTestId}
          name='email' placeholder='email'
          onChange={ev => setEmail(ev.target.value)}
          onBlur={ev => this.setEmailValidity(ev.target.value)}
          size="12"
          maxLength="50"
          description=''
          message={invalidEmailMessage}
        />
        <BareTextInput
          data-testid={nicknameTestId}
          name='nickname' placeholder='nickname'
          onChange={ev => setNickname(ev.target.value)}
          onBlur={ev => this.setNicknameValidity(ev.target.value)}
          size="12"
          maxLength="12"
          description={validNickname}
          message={invalidNicknameMessage}
        />
      </div>

      <div>
        <Button style={{width: '150px'}}
          data-testid={signUpButtonTestId}
          className='mt-3' size="lg" variant="success"
          onClick={startSignUp}>
          Sign Up
        </Button>
        <br />
        <br />
      </div>
      </div>
    )
  };

  render = () => {
    const testingControls= [
      testingControlName.unrecoverable,
      testingControlName.timeout
    ];
    return (
      <ServiceProcessingDecorator
        comp={this}
        testingControls={testingControls}
        errorCallback={() => this.props.done(null)}
      >
        <this.RequestSignUp/><br />
      </ServiceProcessingDecorator>
    )
  };

  // TODO. Validate email, nickname.
  signUp(email, nickname) {
    // let effects = {mockEffects: opMockEffects(this)};
    let mockEffects = opMockEffects(this);
    let resultPromise = serviceStateSettingInterceptor(this, initSignUpDisplay,
      this.authService, this.authService.initSignUp, email, nickname, mockEffects);
    resultPromise.then(resultMapValue((data) => this.props.done(data.clientId, nickname)));
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

export default connect(mapStateToProps, mapDispatchToProps)(SignUpStartComponent);
