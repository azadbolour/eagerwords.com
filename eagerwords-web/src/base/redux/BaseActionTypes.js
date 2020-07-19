/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

const project = 'eagerwords';

const authAction = 'auth';
const configAction = 'config';
const serverAction = 'server';

export const baseActionTypes = {
  // TODO. app config is deprecated. Using environments instead. Update as appropriate.
  appConfigChanged: `${project}/${configAction}/APP_CONFIG_CHANGED`,
  // TODO. Remove action types or move to passwordless Auth subsystem.
  authChanged: `${project}/${authAction}/AUTH_CHANGED`,
  loggedInAsGuest: `${project}/${authAction}/LOGGED_IN_AS_GUEST`,
  loggedOut: `${project}/${authAction}/LOGGED_OUT`,

  // Keep this action type.
  serverInfo: `${project}/${serverAction}/INFO`
};
