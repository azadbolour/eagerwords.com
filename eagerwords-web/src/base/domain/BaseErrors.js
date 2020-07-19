/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

// Replacement for ApiResult which is deprecated.
// TODO. Reflect the doc comments from ApiResult here.

import {stringify} from "../util/Logger";
import {valueResult, errorResult} from './Result';

// TODO. User-friendly message for login expiration.
// "Your secure login period has expired. Please renew your login."

// TODO. Update comments to reflect latest refactorings.

/*
 Results for Asynchronous Calls to the API

 HTTP responses from API calls are converted to a standard Result shape.
 The Result shape is basically an Either. It contains a field 'ok'
 that discriminates between good values and errors.

 The Result shape will always have a field 'json' that includes either the good
 value or the error.

 By default, the good value is just what comes back from the API. The API
 documentation will describe that.

 All errors, including HTTP status errors, API rejections, and normal
 errors transmitted via status 422 (unprocessable entity) are converted
 to a standard error result form.

 The error result form has the following shape:

   {
     tag: a string that uniquely identifies the type of error
     message: the error message
     field, ..., field: specific data about the particular error
  }

  This form is expected to be returned from the server in case
  the error is under program control. Of course, some errors
  are generated and automatically transmitted by HTTP server
  frameworks. These latter types of errors will be recognized by
  their HTTP status.

  If the response is not OK, the HTTP statusText and json, if any,
  is converted to a standard error shape.

  If an API call is rejected for some reason, e.g., a timeout, the rejection
  is converted to a standard error. For now we'll just have a specific error
  tag 'timeout', and a catchall generic error tag 'api' for other rejections.
  The message will just be the rejection reason.
 */

export const errorClassifiers = {
  warning: 'warning',
  loggedOut: 'loggedOut',
  recoverable: 'recoverable',
  unrecoverable: 'unrecoverable',
};

export const basePart = "base";

export const errorTags = {
  // UI generated errors.
  rejection: 'Rejection',        // UI code rejection of an api call.
  timeout: 'TimeoutError',       // UI wait timeout for an async call.
  forcedLoggedOutError: 'forcedLoggedOutError', // Mocked login expiration.
  forcedError: 'forcedError', // Mocked login expiration.

  // HTTP status errors.
  notFound: 'NotFoundError',
  badRequest: 'BadRequestError',
  internalError: 'InternalError',
  unprocessable: 'UnprocessableEntityError',  // Catchall for unrecognized 422 error.

  // Catchall.
  unclassified: 'Unclassified',
};

export const baseClassifiers = {
  [errorTags.rejection]: errorClassifiers.unrecoverable,
  [errorTags.timeout]: errorClassifiers.unrecoverable,
  [errorTags.forcedError]: errorClassifiers.unrecoverable,
  [errorTags.forcedLoggedOutError]: errorClassifiers.loggedOut,
  [errorTags.notFound]: errorClassifiers.unrecoverable,
  [errorTags.badRequest]: errorClassifiers.unrecoverable,
  [errorTags.internalError]: errorClassifiers.unrecoverable,
  [errorTags.unprocessable]: errorClassifiers.recoverable,
  [errorTags.unclassified]: errorClassifiers.unrecoverable,
};

// TODO. Add explanation to errors. Details of how it happened and what to do to recover.
// The error modal will include the message as header and the explanation as body text.
export const mkErrorData = function(part, tag, classifier, message) {
  return {part, tag, classifier, message};
};

/**
 * Convert an error json returned from a 422 (unprocessable) response to a message.
 * Can be specialized for each subsystem ('part').
 */
export const defaultMessageTransformer = (errorJson) => {
  console.log(`default transformer - errorJson: ${stringify(errorJson)}`);
  let message = errorJson && errorJson.message ? errorJson.message : "internal error";
  console.log(`message: ${stringify(message)}`);
  return message;
};

// At a later time may wish to improve upon the server-side messages.
export const baseMessageTransformer = defaultMessageTransformer;

export const isWarningError = (error) => error.classifier === errorClassifiers.warning;
export const isLoggedOutError = (error) => error.classifier === errorClassifiers.loggedOut;
export const isRecoverableError = (error) => error.classifier === errorClassifiers.recoverable;
export const isUnrecoverableError = (error) => error.classifier === errorClassifiers.unrecoverable;

export const isWarningResult = (result) => !result.ok && isWarningError(result.data);
export const isLoggedOutResult = (result) => !result.ok && isLoggedOutError(result.data);
export const isRecoverableResult = (result) => !result.ok && isRecoverableError(result.data);
export const isUnrecoverableResult = (result) => !result.ok && isUnrecoverableError(result.data);

export const mkTimeoutError = function(message) {
  return mkErrorData(basePart, errorTags.timeout, baseClassifiers[errorTags.timeout], message);
};

export const mkForcedError = function(message) {
  return mkErrorData(basePart, errorTags.forcedError, baseClassifiers[errorTags.forcedError], message);
};

export const mkForcedLoggedOutError = function(message) {
  return mkErrorData(basePart, errorTags.forcedLoggedOutError, baseClassifiers[errorTags.forcedLoggedOutError], message);
};

export const rejectedResponseToError = (part, reason) => {
  // Is reason is an already-constructed error result.
  // Assume so if it has our error structure. This is a hack for expedience.
  if (reason && reason.data && (reason.ok === true) &&
    reason.data.tag && reason.data.message && reason.data.classifier)
    return reason;
  else {
    let classifier = baseClassifiers[errorTags.rejection];
    let message = `async operation rejected - ${stringify(reason)}`;
    return mkErrorData(part, errorTags.rejection, classifier, message);
  }
};

/**
 * Return a conversion function from a promise of an HTTP response to a promise of Result,
 * based on a specific error map.
 */
export const mkResponseToResultPromiseMapper =  (part, errorClassifier, messageTransformer) => {

  const resolvedResponseToError = (response) => {
    // console.log(`encountered error: ${stringify(response)}`);
    switch (response.status) {
      case 422:
        return unprocessableEntityToError(response);
      case 400:
        return statusResponseToError(response, errorTags.badRequest, 'malformed server request');
      case 404:
        return statusResponseToError(response, errorTags.notFound, 'server request target not found');
      case 500:
        return statusResponseToError(response, errorTags.internalError, 'internal server error');
      default:
        return statusResponseToError(response, errorTags.unclassified, 'internal server error');
    }
  };

  /**
   * Convert a 422 error to an error result.
   * The API contract requires that 422 errors include a compliant json.
   * It will include at least a tag and a message.
   */
  const unprocessableEntityToError = (response) => {
    let json = response.json;
    let tag = (json && json.tag) ? json.tag : errorTags.unprocessable;
    let classifier = errorClassifier[tag];
    let message = messageTransformer(json);
    return mkErrorData(part, tag, classifier, message);
  };

  const statusResponseToError = (response, tag, message) => {
    let classifier = errorClassifier[tag];
    message = (response.json) ? stringify(response.json) : message;
    return mkErrorData(part, tag, classifier, message);
  };


  let mkResult = (responsePromise) => {
    return (
      responsePromise.then(response => {
        // console.log(`response: ${stringify(response)}`);
        return response.ok ?
          valueResult(response.json) :
          errorResult(resolvedResponseToError(response))
      }).catch(reason => {
        // return {ok: false, data: rejectedResponseToError(part, reason)}
        return errorResult(rejectedResponseToError(part, reason));
      })
    )
  };
  return mkResult;
};

export const resultErrorMessage = (result) => result.data.message;

// Deprecated. Use Vow.
export const mapResultPromise = (resultPromise, func) => {
  return resultPromise.then(result => {
    // return mapResult(result, func)
    return result.mapValue(func);
  });
};


