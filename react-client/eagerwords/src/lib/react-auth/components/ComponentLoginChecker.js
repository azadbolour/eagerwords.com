/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {ComponentOperationState} from 'lib/react-support/index';
import {ComponentProcessingStateManager} from 'lib/react-support/index';
import {authService, isLoggedInDisplay} from "../service/AuthService";

let {serviceStateSettingInterceptor} = ComponentProcessingStateManager;
let {setOpLoggedOutMsg} = ComponentOperationState;

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
