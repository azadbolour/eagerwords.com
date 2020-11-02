/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

// TODO. Move to auth/util.

import {BaseErrors} from 'lib/js-util/index';

let {
  errorClassifiers,
  defaultMessageTransformer,
  mkResponseToResultPromiseMapper,
  baseClassifiers
} = BaseErrors;

// TODO. How to avoid name clashes for parts?
export const authPart = "auth";

export const errorTags = {
  notSignedUp: 'NotSignedUpError',
  alreadyConfirmed: 'AlreadyConfirmedError',
  confirmationTimeout: 'ConfirmationTimeoutError',
  alreadySignedUp: 'AlreadySignedUpError',
  invalidEmail: 'InvalidEmailError',
  invalidNickname: 'InvalidNicknameError',
  missingAuthEvidence: 'MissingAuthEvidenceError',
  loginExpired: 'LoginExpiredError',
  signUpConfirmationWithoutInitialization: 'SignUpConfirmationWithoutInitializationError',
  loginConfirmationWithoutInitialization: 'LoginConfirmationWithoutInitializationError',
  confirmationTokenMismatch: 'ConfirmationTokenMismatchError',
};

// TODO. URGENT. Check for recoverable (alertMessage) in components and display in status bar.

export const authClassifiers = {
  [errorTags.notSignedUp]: errorClassifiers.unrecoverable,
  [errorTags.alreadyConfirmed]: errorClassifiers.unrecoverable,
  [errorTags.alreadySignedUp]: errorClassifiers.unrecoverable,
  [errorTags.invalidEmail]: errorClassifiers.recoverable,
  [errorTags.invalidNickname]: errorClassifiers.recoverable,
  [errorTags.missingAuthEvidence]: errorClassifiers.loggedOut,
  [errorTags.loginExpired]: errorClassifiers.loggedOut,
  [errorTags.signUpConfirmationWithoutInitialization]: errorClassifiers.unrecoverable,
  [errorTags.loginConfirmationWithoutInitialization]: errorClassifiers.unrecoverable,
  [errorTags.confirmationTokenMismatch]: errorClassifiers.unrecoverable,
};

// At a later time may wish to improve upon the server-side messages.
export const authMessageTransformer = defaultMessageTransformer;

export const authResponseToResultPromiseMapper =
  mkResponseToResultPromiseMapper(authPart, {...baseClassifiers, ...authClassifiers}, authMessageTransformer);
