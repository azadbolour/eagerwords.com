/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/*
 Environment variables, their values, and their defaults.

 Initially we just use local environment files. They are all gitignored in
 case they include secrets. Therefore, deployments to production will have to
 be made from a secure machine by using corresponding env files that are not
 in source control.

 For now we are not using any env secrets, and we are also not saving any env
 files in source control. The production deployment will just get the default
 values of the env variables defined here.
 */

import {getEnv} from "./MiscUtil";
const validUrl = require('valid-url');

export const ENV_UPDATE_DATE = 'REACT_APP_UPDATE_DATE';
export const ENV_SERVER_PORT = 'REACT_APP_SERVER_PORT';
export const ENV_MOCK_EMAIL = 'REACT_APP_MOCK_EMAIL';
export const ENV_MOCK_TOKEN = 'REACT_APP_MOCK_TOKEN';
export const ENV_REACT_APP_USE_MOCK_TESTING_COMPONENT = 'REACT_APP_USE_MOCK_TESTING_COMPONENT';

export const defaultHostName = 'localhost';

// No port is used in production deployment!
// For local deployment, need explicit port.
export const defaultServerPort = '';
export const defaultTestServerPort = '6587';
export const getServerPort = () => getEnv(ENV_SERVER_PORT, defaultServerPort);

export const hostname = window.location.hostname;
// Note: location.protocol includes the final colon!
export const protocol = window.location.protocol;

/**
 * Build the server url.
 *
 * @param protocol Includes the final colon! 'http:', 'https:'.
 * @param hostname The host part.
 * @param port The port. Maybe a number or just '' for now port.
 */
export const mkServerUrl = function(protocol, hostname, port) {
  let portPart = port ? `:${port}` : '';
  return `${protocol}//${hostname}${portPart}`;
};

export const getServerUrl = () => {
  let port = getServerPort();
  let url = mkServerUrl(protocol, hostname, port);
  return url;
};

export const defaultServerUrl = mkServerUrl('http', defaultHostName, defaultServerPort);

export const getUpdateDate = () => getEnv(ENV_UPDATE_DATE, 'Oct 31, 2020');
export const getMockEmail = () => getEnv(ENV_MOCK_EMAIL);
export const getMockToken = () => getEnv(ENV_MOCK_TOKEN);

export const getUseMockTestingComponent = () => {
  let string = getEnv(ENV_REACT_APP_USE_MOCK_TESTING_COMPONENT, 'false');
  return string.toLowerCase() === 'true';
};

// TODO. URGENT. Validate all env variables.
const validated = {valid: true};

// TODO. Change to validate server port.
const validateServerUrl = function(url) {
  // Note. isWebUri returns the uri if valid, undefined if not.
  let valid = validUrl.isWebUri(url) !== undefined;
  if (!valid)
    return {
      valid: false,
      message: `invalid url ${url}`

    };
  return validated;
};
