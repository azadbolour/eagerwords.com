/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import '@testing-library/jest-dom'
import user from '@testing-library/user-event'
import React from 'react'
import {render, fireEvent, screen, waitForElement} from '@testing-library/react'
import LandingComponent from '../../auth/components/LandingComponent';
// TODO. URGENT. Undefined import. Fix.
import {landingHeading} from '../../auth/components/LandingComponent';
import {authInitialState} from "../../auth/redux/AuthReducer";
import {StoreProvider} from "./StoreProvider";
// TODO, emailTestId is duplicated for components. Discriminate.
import {
  gotoSignUpButtonTestId,
  signInButtonTestId,
  emailTestId
} from "../../auth/components/LandingComponent";
import {signUpButtonTestId, nicknameTestId} from "../../auth/components/SignUpStartComponent";
import {confirmSignInButtonTestId, tokenTestId} from "../../auth/components/SignInConfirmComponent";
import {unrecoverableCheckboxTestId} from "../../base/components/Testing2Components";
import {unrecoverableResetButtonTestId} from "../../base/components/NotificationComponents";
import {stringify} from "../util/Logger";

// TODO. URGENT. Incomplete work. Two tests are skipped. One does not work.

const userEvent = user; // User is an overloaded name. How to use alias??

const preLoadedState = {
  authReducer: authInitialState
};

const emailAddress = 'bill@example.com';
const nickname = 'bill';

const renderLanding = function() {
  let result = render(
    <StoreProvider preLoadedState={preLoadedState}>
      <LandingComponent/>
    </StoreProvider>
  );
  // TODO. URGENT. Undefined variable. Fix.
  expect(screen.getByText(landingHeading)).toBeInTheDocument();
  return result;
};

// TODO. This test is incomplete and fails even in its incomplete stage.
// Probably because we need to render the complete App for it to work.
// Once it works, extract its code into a function and use it in the login tests below
// to sign up a user first.
test('happy sign-up sequence', async () => {

  // Render the landing page.
  let {getByTestId} = renderLanding();
  let elementByTestId = async function (testId) {
    return await waitForElement(() => getByTestId(testId));
  };

  let gotoSignUpButton = await elementByTestId(gotoSignUpButtonTestId);
  userEvent.click(gotoSignUpButton);

  // That should take us to the SignUpStartComponent.
  let signUpButton = await elementByTestId(signUpButtonTestId);
  let emailInput = await elementByTestId(emailTestId);
  let nicknameInput = await elementByTestId(nicknameTestId);

  userEvent.change(emailInput, {target: {value: emailAddress}});
  userEvent.change(nicknameInput, {target: {value: nickname}});

});

// test.skip('happy login sequence', async () => {
test.skip('happy login sequence', async () => {

  let {getByTestId} = renderLanding();
  let elementByTestId = async function (testId) {
    return await waitForElement(() => getByTestId(testId));
  };

  /*
   * LandingStartComponent should be rendered but only after the asynchronous
   * check to see if the user is already logged in. So wait for its elements to appear.
   */
  let signInButton = await elementByTestId(signInButtonTestId);
  let emailInput = await elementByTestId(emailTestId);
  fireEvent.change(emailInput, {target: {value: emailAddress}});
  fireEvent.click(signInButton);

  // LandingConfirmComponent similarly is rendered asynchronously.
  let confirmSignInButton = await elementByTestId(confirmSignInButtonTestId);
  let tokenInput = await elementByTestId(tokenTestId);
  fireEvent.change(tokenInput, {target: {value: '12345678'}});
  fireEvent.click(confirmSignInButton);

});

test.skip('request login failure', async () => {
  let {getByTestId} = renderLanding();
  let elementByTestId = async function (testId) {
    return await waitForElement(() => getByTestId(testId));
  };

  /*
   * LandingStartComponent should be rendered but only after the asynchronous
   * check to see if the user is already logged in. So wait for its elements to appear.
   */
  let signInButton = await elementByTestId(signInButtonTestId);
  let emailInput = await elementByTestId(emailTestId);
  fireEvent.change(emailInput, {target: {value: emailAddress}});

  // Force unrecoverable error.
  let unrecoverableCheckbox = await elementByTestId(unrecoverableCheckboxTestId);
  fireEvent.click(unrecoverableCheckbox);
  fireEvent.click(signInButton);

  // Unrecoverable modal should be rendered on top of the landing start page.
  // Reset will go back to the default landing page - and render start landing.
  let unrecoverableResetButton = await elementByTestId(unrecoverableResetButtonTestId);
  console.log(`${unrecoverableResetButton.innerHTML}`);
  fireEvent.click(unrecoverableResetButton);

  // The unrecoverable modal should go away and the landing start page rerendered.
  unrecoverableCheckbox = await elementByTestId(unrecoverableCheckboxTestId);
  expect(unrecoverableCheckbox.checked).toEqual(false);
  emailInput = await elementByTestId(emailTestId);
  expect(emailInput.value).toEqual(emailAddress);
});

// TODO. Test login request times out.
// TODO. Test login confirm fails.
// TODO. Test login confirm times out.

/*
  Notes.
  Options is an optional second parameter to wait functions.
  To add timeout to a wait function use: {timeout: 2000} for options.
  The default timeout is 1000ms which will keep you under Jest's default timeout of 5000ms.
 */
