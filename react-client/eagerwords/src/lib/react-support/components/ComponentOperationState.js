/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {ApiAdapters} from 'lib/js-util/index';

let {defaultMockOpEffects} = ApiAdapters;

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
  loggedOutMsg: null,

  /**
   * The unique error tag for comparisons.
   */
  errorTag: null
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
export const opErrorTag = (comp) => opMessages(comp).errorTag;

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

export const setOpErrorTag = (comp, errorTag) => {
  let messages = {...opMessages(comp), errorTag};
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
