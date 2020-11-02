import {stringify} from "../util/Logger";
import {errorTags} from "./BaseErrors";
import {unHappyResponsePromise} from "../util/HttpUtil";

/**
 * Effects to mock for testing.
 *
 * Mock effects must be set before an operation.
 */
export const defaultMockOpEffects = {
  /**
   * Mock a general unrecoverable error.
   */
  mockError: false,

  /**
   * Mock a timeout.
   */
  mockTimeout: false,

  /**
   * Mock a login expiration error.
   */
  mockLoggedOut: false
};


const mockTimeoutMillis = 2000;

const errorResponse = (tag, message) => unHappyResponsePromise({tag, message}, 422);
const timeoutMessage = (what) => `${what} timed out`;
const mockTimeoutResponse = (what) => errorResponse(errorTags.timeout, timeoutMessage(what));
const mockErrorResponse = errorResponse(errorTags.forcedError, 'mocked error');
const mockLoggedOutResponse = errorResponse(errorTags.forcedLoggedOutError, 'mocked login expiration');

/**
 * Perform an async function that returns a promise of an HTTP response
 * data structure, mocking certain errors if so directed via the
 * mockErrors parameter.
 *
 * The return is always a promise of an object that includes
 * at least an ok field, a json field, and a status field.
 *
 * If no mocked error is specified the given function is called
 * and its return is returned verbatim.
 *
 * For mocked errors, a response promise is returned that includes
 * a status field of 422, and a json object that includes
 * standard error data, namely a tag and a message.
 *
 * @param api The api implementation object.
 * @param func A function of no argument returning a promise.
 * @params args The function's arguments.
 * @param description The user-readable description of the function.
 * @param mockErrors - An object that includes a boolean for each type error to be mocked.
 *        Only one mock error is used.
 *
 * @returns The returned promise from the function call or a promise
 *          containing a response indicating a 422 error.
 */
// TODO. Can use Proxy for this.
export const apiMockErrorAdapter = (description, mockErrors = defaultMockOpEffects) => (api, func, ...args) => {
  if (mockErrors.mockTimeout)
    return new Promise((resolve, reject) =>
      setTimeout(() => resolve(mockTimeoutResponse(description)), mockTimeoutMillis)
    );
  if (mockErrors.mockError)
    return Promise.resolve(mockErrorResponse);

  if (mockErrors.mockLoggedOut)
    return Promise.resolve(mockLoggedOutResponse);

  return func.apply(api, args);
};


