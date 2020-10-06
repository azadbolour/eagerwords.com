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

// The API implementation to be used.
import {getEnv} from "./base/util/MiscUtil";
import {stringify} from "./base/util/Logger";
const validUrl = require('valid-url');

export const ENV_API_TYPE = 'REACT_APP_API';
export const ENV_SERVER_URL = 'REACT_APP_SERVER_URL';
export const ENV_SERVER_PORT = 'REACT_APP_SERVER_PORT';
export const ENV_MOCK_EMAIL = 'REACT_APP_MOCK_EMAIL';
export const ENV_MOCK_TOKEN = 'REACT_APP_MOCK_TOKEN';
export const ENV_REACT_APP_USE_MOCK_TESTING_COMPONENT = 'REACT_APP_USE_MOCK_TESTING_COMPONENT';

// Valid values of for the API.
export const apiTypes = {
  mock: 'mock',
  client: 'client'
};

export const defaultApiType = apiTypes.client;

export const getApiType = () => getEnv(ENV_API_TYPE, defaultApiType);

export const defaultServerPort = 6587;
export const getServerPort = () => getEnv(ENV_SERVER_PORT, defaultServerPort);

// Deprecated.
export const defaultServerUrl = 'http://localhost:6587';

export const hostname = window.location.hostname;
export const protocol = window.location.protocol;
export const getServerUrl = () => {
  let port = getServerPort();
  let url = `${protocol}//${hostname}:${port}`;
  // console.log(`protocol: ${protocol}`);
  // console.log(`hostname: ${hostname}`);
  // console.log(`url: ${url}`);
  return url;
};

// export const getServerUrl = () => getEnv(ENV_SERVER_URL, defaultServerUrl);

export const getMockEmail = () => getEnv(ENV_MOCK_EMAIL);
export const getMockToken = () => getEnv(ENV_MOCK_TOKEN);

export const getUseMockTestingComponent = () => {
  let string = getEnv(ENV_REACT_APP_USE_MOCK_TESTING_COMPONENT, 'false');
  return string.toLowerCase() === 'true';
};

// TODO. URGENT. Validate all env variables.
const validated = {valid: true};

const validateApiType = function(apiType) {
  let valid = apiTypes[apiType];
  if (!valid)
    return {
      valid: false,
      message: `invalid api-type ${apiType} - valid values are ${stringify(apiTypes)}`
    };
  return validated;
};

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
