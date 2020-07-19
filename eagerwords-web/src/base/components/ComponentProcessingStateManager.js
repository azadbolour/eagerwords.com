/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React from "react";

import {getUseMockTestingComponent} from '../../envvars';
import {
  LoggedOutModal,
  ProcessingSpinnerModal,
  UnrecoverableModal
} from "./NotificationComponents";

import {TestingControls} from "./Testing2Components";
import {stringify} from "../util/Logger";
import {
  isLoggedOutResult,
  isRecoverableResult,
  isWarningResult,
  resultErrorMessage,
} from "../domain/BaseErrors";
import {authService, isLoggedInDisplay} from "../../auth/service/AuthService";
import {defaultMockOpEffects} from '../domain/ApiAdapters';

/*
 Generic state management for components that make asynchronous service calls.

 When a components calls an asynchronous service layer operation, various
 standard effects are needed in the user interface to inform the user of
 the progress and the result of the operation. This module abstracts those
 effects by providing two constructs: the function serviceStateSettingInterceptor,
 and the components ServiceProcessingDecorator.

 The serviceStateSettingInterceptor wraps calls from a component to the service layer,
 and sets state variables in the component to indicate the progress of the
 call and the type of error (if any) returned by the call.

 The ServiceProcessingDecorator is a component renders generic processing
 and error handling content based on the state of a component set in
 serviceStateSettingInterceptor.

 A component that uses the operation context machinery for generic rendering
 will wrap its specific rendered content (generally based on the happy responses
 from the service layer) within an ServiceProcessingDecorator, delegating to it
 the rendering of processing and error status.

 A component that adheres to the operation context contract will have a state
 variable called opContext, whose shape is given in 'defaultOpContext'. It makes
 calls to the service layer through the generic serviceStateSettingInterceptor,
 and it encloses its happy rendered content inside an ServiceProcessingDecorator.
 */

/*
 Summary of the requirements of the operation context contract.

 1. The component defines a state variable called opContext with
    properties as defined in defaultOpContext defined in this module.
    This allows the generic machinery for making service calls to
    'communicate' the state of the service call to the component,
    so that the generic UI rendering can occur for processing events.

 2. In order for service calls to set appropriate opContext state in the
    component, service calls from the component are wrapped in
    serviceStateSettingInterceptor. The interceptor will set processing
    and standard error messages in the opContext.

 3. The component renderer wraps the contents of the component in
    a ServiceProcessingDecorator component. This decorator component
    will check the opContext for the occurrence of processing events,
    and render appropriate modals for processing waits, and for various
    error conditions.

 4. The decorator component requires callbacks for two types of error
    errors, login expiration, and general unrecoverable errors. These
    properties of the decorator component are called respectively
    'errorCallback', and 'loginExpiredCallback'.

 5. It is the responsibility of loginExpiredCallback to reset the
    login evidence in the redux store to indicate a logged out
    UI state, and to redirect the user to the entry component.

 6. The current opContext contract also requires the errorCallback to
    reset the redux store's login evidence and redirect to the entry
    component. In this way we simply escape to safety in case of
    an unrecoverable error.

 7. The contract for the entry component is that if it finds a valid
    login evidence in the redux store, it assumes, optimistically,
    that the user is logged in, and simply redirects the 'home component'
    of the application. The home component has to recheck the validity of
    login with the server anyway, and if it finds that login has expired,
    it would reset the login evidence in the redux store as explained above.

  Admittedly this is a complex and error-prone contract. It is our current
  compromise between keeping track of logins so user's don't have to
  unnecessarily relogin, on the one hand, and the generic processing of
  expired logins. Subject to review in following versions of the
  authentication model.

 */


/**
 * Messages related to the last action attempted in the components.
 * They indicate whether the action is in progress, whether login
 * was found to have expired in performing the action, and whether
 * an unrecoverable error was encountered during the action.
 * Messages must be rest before the operation.
 */
export const defaultOpMessages = {

  /**
   * Non-null value is what to tell the user while waiting for an
   * action to complete.
   */
  processingMsg: null,

  /**
   * The unrecoverable error, if any, resulting from the previous action.
   * The error would be rendered and the user is allowed to see the error
   * and reset the application.
   */
  errorMsg: null,

  /**
   * The user needs to be informed about something related to the last operation.
   * A simple user error. What to do next. And so on.
   */
  alertMsg: null,

  /**
   * Login has expired.
   */
  loggedOutMsg: null
};


export const defaultOpContext = {
  messages: defaultOpMessages,
  mockEffects: defaultMockOpEffects
};

// Shorthands for selections.

export const opContext = (comp) => comp.state.opContext;

export const opMessages = (comp) => opContext(comp).messages;
export const opProcessingMsg = (comp) => opMessages(comp).processingMsg;
export const opErrorMsg = (comp) => opMessages(comp).errorMsg;
export const opAlertMsg = (comp) => opMessages(comp).alertMsg;
export const opLoggedOutMsg = (comp) => opMessages(comp).loggedOutMsg;

export const opMockEffects = (comp) => opContext(comp).mockEffects;
export const opMockError = (comp) => opMockEffects(comp).mockError;
export const opMockTimeout = (comp) => opMockEffects(comp).mockTimeout;
export const opMockLoggedOut = (comp) => opMockEffects(comp).mockLoggedOut;

// Setters.

export const setOpContext = (comp, opContext) => {
  let setter = (state) => { return {...state, opContext} };
  comp.setState(setter);
};

export const setOpMessages = (comp, messages) => {
  let context = {...opContext(comp), messages};
  setOpContext(comp, context);
};

export const setOpMockEffects = (comp, mockEffects) => {
  let context = {...opContext(comp), mockEffects};
  setOpContext(comp, context);
};

export const setOpProcessingMsg = (comp, processingMsg) => {
  let messages = {...opMessages(comp), processingMsg};
  setOpMessages(comp, messages);
};

export const setOpErrorMsg = (comp, errorMsg) => {
  let messages = {...opMessages(comp), errorMsg};
  setOpMessages(comp, messages);
};

export const setOpAlertMsg = (comp, alertMsg) => {
  let messages = {...opMessages(comp), alertMsg};
  setOpMessages(comp, messages);
};

export const setOpLoggedOutMsg = (comp, loggedOutMsg) => {
  let messages = {...opMessages(comp), loggedOutMsg};
  setOpMessages(comp, messages);
};

export const setOpMockError = (comp, mockError) => {
  let mockEffects = {...opMockEffects(comp), mockError};
  setOpMockEffects(comp, mockEffects);
};

export const setOpMockTimeout = (comp, mockTimeout) => {
  let mockEffects = {...opMockEffects(comp), mockTimeout};
  setOpMockEffects(comp, mockEffects);
};

export const setOpMockLoggedOut = (comp, mockLoggedOut) => {
  let mockEffects = {...opMockEffects(comp), mockLoggedOut};
  setOpMockEffects(comp, mockEffects);
};

export const resetOpMessages = (comp) => setOpMessages(comp, defaultOpMessages);
export const resetMockOpEffects = (comp) => setOpMockEffects(comp, defaultMockOpEffects);
export const resetOpContext = (comp) => setOpContext(comp, defaultOpContext);

/**
 * Run an asynchronous action and set component state before
 * the action to reflect processing, and after the action to reflect
 * errors. Rejections are converted to error Results by this function.
 *
 * @param comp The components calling the action.
 * @param actionDisplayText The name of the action.
 * @param action An action returning a promise of a Result (see ApiResult).
 *
 * @returns The result of the operation as a Result data structure.
 */
export const serviceStateSettingInterceptor = (comp, actionDisplayText, service, func, ...args) => {
  resetOpMessages(comp);
  let timeout = setTimeout(() => setOpProcessingMsg(comp,`processing: ${actionDisplayText} ...`), 250);
  return func.apply(service, args).then(result => {
    clearTimeout(timeout);
    resetOpContext(comp);
    // console.log(`result of ${actionDisplayText}: ${stringify(result)}`);
    if (!result.ok) {
      let message = resultErrorMessage(result);
      message = `${actionDisplayText}: ${message}`;
      if (isLoggedOutResult(result))
        setOpLoggedOutMsg(comp, `${message}. Please log in again.`);
      else if (!isRecoverableResult(result))
        setOpErrorMsg(comp, message);
      else if (isWarningResult(result))
        console.log(`warning: ${message}`);
      else // Recoverable error. Specific component code will display it.
        setOpAlertMsg(comp, message);
    }
    return result;
  })
};

/**
 * Generic wrapper component for rendering processing messages,
 * error messages, and alerts, based on the state of the components
 * before and after an operation.
 *
 * The real action occurs in this components's children. This component
 * decorates its children with generic widgets associated with
 * waiting for asynchronous responses, error processing, and testing.
 *
 * MessageArea shows user errors.
 *
 * UnrecoverableModal notifies the user of system errors.
 *
 * LoggedOutModal tells the user that he is not (or no longer) logged in.
 *
 * ProcessingSpinner is used for waiting for async calls.
 *
 * Testing controls are a development-only feature to force various conditions
 * in testing.
 *
 * @param props
 *
 *  comp - required - the target components that include this one
 *         the target components is expected to include OperationState in its state
 *  errorCallback - optional - callback function to indicate error after
 *                  presenting an error modal to the user and having the user dismiss it
 *  loginExpiredCallback - optional - callback function to indicate error after
 *                         presenting an error modal to the user and having the
 *                         user dismiss it
 */
export const ServiceProcessingDecorator = (props) => {
  let comp = props.comp;
  let testingControls = getUseMockTestingComponent() ? props.testingControls : null;
  let state = comp.state;
  let opContext = state.opContext;
  let opMessages = opContext.messages;
  let processingMsg = opMessages.processingMsg;
  let errorMsg = opMessages.errorMsg;
  let loggedOutMsg = opMessages.loggedOutMsg;

  let isError = errorMsg !== null;
  let showProcessing = processingMsg !== null;
  let showLoggedOut = loggedOutMsg !== null;

  let loggedOutReset = () => {
    console.log(`loginExpiredReset - loginExpiredCallback is set: ${props.loginExpiredCallback ? "yes" : "no"}`);
    // If login expired callback is not given, it must be because that error type is not expected.
    // To cover an unexpected mistake - treat stray login expired errors as unrecoverable errors.
    resetOpMessages(comp);
    // TODO. URGENT. Change loginExpired to loggedOut everywhere.
    let callback = (props.loginExpiredCallback) ? props.loginExpiredCallback : props.errorCallback;
    if (callback) {
      console.log(`calling callback`);
      callback();
    }
  };

  let errorReset = () => {
    resetOpMessages(comp);
    if (props.errorCallback)
      props.errorCallback();
  };

  let Testing = () =>
      <div style={{display: 'flex', flexDirection: 'row'}}>
        <div style={{'backgroundColor': 'WhiteSmoke', padding: '10px'}}>
          <TestingControls comp={comp} controls={testingControls} />
        </div>
      </div>;

  return (
    <div>
      <LoggedOutModal
        show={showLoggedOut}
        message={loggedOutMsg}
        reset={() => loggedOutReset()}
      />
      <UnrecoverableModal
        show={isError}
        message={errorMsg}
        reset={() => errorReset()}
      />
      <ProcessingSpinnerModal
        show={showProcessing}
        message={processingMsg}
      />
      {props.children}
      {testingControls && <Testing />}
    </div>
  )
};

/**
 * Check that the user is logged in. This function needs the
 * component invoking the login check as a parameter. The component
 * must adhere to the operation context contract. The component's
 * processing and error states will be set by this function according
 * to the operation context contract.
 *
 * @param comp The component in which the check is invoked.
 */
export const checkLoggedIn = (comp) => {
  // TODO. Get error from base errors.
  let expiredMessage = `Login expired. Please log in again.`;
  let resultPromise = serviceStateSettingInterceptor(comp, isLoggedInDisplay,
    authService, authService.isLoggedIn, comp.props.loginEvidence);
  return resultPromise.then(result => {
    let isLoggedIn = result.data;
    isLoggedIn || setOpLoggedOutMsg(comp, expiredMessage);
    return isLoggedIn;
  });
};