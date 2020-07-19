/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

const project ='eagerwords';

const authAction = 'auth';

export const authActionTypes = {
  enteredAsGuest: `${project}/${authAction}/ENTERED_AS_GUEST`,
  loginConfirmed: `${project}/${authAction}/LOGIN_CONFIRMED`,
  signUpConfirmed: `${project}/${authAction}/SIGN_UP_CONFIRMED`,
  loggedOut: `${project}/${authAction}/LOGGED_OUT`,
};