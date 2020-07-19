/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import {stringify} from "./Logger";
import {toast} from 'react-toastify';

export const coinToss = (x, y) => Math.random() < 0.5 ? x : y;

export const randomString = function() {
  return Math.random().toString().substring(2);
};

export const checkArray = function(array, message) {
  if (!Array.isArray(array))
    throw {
      name: "not an array",
      message: `${message}: object: ${stringify(array)}`
    };
};

export const checkArrayIndex = function(index, size, message) {
  if (index >= size || index < 0)
    throw {
      name: "index out of bounds",
      message: `${message} - index: ${index}, size: ${size}`
    };
};

export const checkCapacityOverflow = function(size, capacity, message) {
  if (size > capacity)
    throw {
      name: "capacity overflow",
      message: `${message} - required size: ${size}, capacity: ${capacity}`
    };
};

export const zipWith = function(arr1, arr2, zipper) {
  checkArray(arr1, "cannot zip non-array");
  checkArray(arr2, "cannot zip non-array");

  let len = Math.min(arr1.length, arr2.length);

  let zipped = [];
  for (let i = 0; i < len; i++)
    zipped.push(zipper(arr1[i], arr2[i]));

  return zipped;
};

/*
 * Given a function `f` and integer `n`, return an array consisting of the values
 * `[f(0), f(1), ... f(n)]`
 */

export const repeatF = function(n, f) {
  let ret = [];
  for (let i = 0; i < n; i++) {
    ret.push(f(i));
  }
  return ret;
};

export const range = function(n) {
  return Array.from({length: n}, (v, i) => i);
};


export const mkErrorState = function() {
  let _error = false;
  let _status = "";
  return {
    get error() { return _error; },
    get status() { return _status; },
    addError: function (message) {
      _status += _error ? "\n" : "";
      _error = true;
      _status += message;
    },
    addErrorState: function(errorState) {
      if (errorState.error) {
        this.addError(errorState.status)
      }
    }
  }
};

export const toCamelCase = function(name) {
  let camelName = name.replace(/(-)([a-z])/g, function(match, dash, initial, offset, string) { return initial.toUpperCase()});
  return camelName;
};

export const getOrElse = function(object, field, defaultValue) {
  const value = object[field];
  return (value !== undefined) ? value : defaultValue;
};

export const safeGetOrElse = (object, field, defaultValue) => {
  if (!object)
    return defaultValue;
  let value = object[field];
  return (value !== undefined && value != null) ? value : defaultValue;
};

export const getOrElseF = function(object, field, defaultFunction) {
  const value = object[field];
  return (value !== undefined) ? value : defaultFunction();
};

export const ifEmpty = function(value, defaultValue) {
  return (value === undefined || value === null || Object.keys(value).length === 0) ? defaultValue : value;
};

export const getEnv = function(varName, defaultValue) {
  let value = process.env[varName];
  return value ? value : defaultValue;
};

export const strToNumber = function(str) {
  return (/[0-9]+/).test ? Number(str) : NaN;
};

export const isEmpty = function(obj) {
  for(let key in obj) {
    if(obj.hasOwnProperty(key))
      return false;
  }
  return true;
};

/* `error`, `errorQuiet` and `warn` are used for non-user errors.
 * Logs the error to the console. Argument string are joined on space.
 * `error` shows an error bubble with a generic message.
 * `warn` and `errorQuiet` don't show the bubble.
 * Returns `undefined`.
 */
const _problem = (consoleMethod, showToast) => (...msg) => {
  let theMsg = msg.join(' ');
  if (showToast) {
    toast.dismiss();
    toast('Oops! Something went wrong. Please try again later.');
  }
  console[consoleMethod](theMsg);
};

export const error      = _problem('error', true);
export const errorQuiet = _problem('error', false);
export const warn       = _problem('warn', false);

/* Used for user errors.
 * Logs an internal message ('imsg') to the console and shows a user-friendly message ('umsg') in an
 * error bubble.
 * Argument strings are joined on space.
 *
 * Usage:
 *
 * If imsg and umsg are the same, call like:
 *     uerror('Error:', str1, str2, ...);
 *
 * If they're different:
 *     uerror([
 *       ['Oops! Please tray again', str1, str2, ...], // umsg
 *       ['Error getting user id', userId, 'the server said', str1, str2, ...], // imsg
 *     ]);
 *
 * Returns `undefined`.
 */
export const uerror = function(...msgs) {
  let umsg, imsg;
  if (Array.isArray(msgs[0]))
    [umsg, imsg] = msgs[0];
  else
    umsg = imsg = msgs;
  let theUmsg = umsg.join(' ');
  let theImsg = imsg.join(' ');
  toast.dismiss();
  toast(theUmsg);
  console.error('[user error]', theImsg);
};

export const nil = function(x) {
  return x === undefined || x === null;
};

/* Reverse the keys and values of an object.
 * Note that the result may be unpredictable if the passed object contains duplicate values.
 */
export const reverseObject = function(obj) {
  const reversed = {};
  for (const key in obj) {
    reversed[obj[key]] = key;
  }
  return reversed;
};

// Deprecated. Use timedAsyncCall.
export const timedPromise = (promise, timeoutMillis, actionName) => {
  console.log(`timedPromise - timeoutMillis: ${timeoutMillis}`);
  let timeoutReason = `Execution of ${actionName} timed out after ${timeoutMillis} milliseconds.`;
  let timeoutRejectedPromise = new Promise((resolve, reject) =>
    setTimeout(() => reject(timeoutReason), timeoutMillis)
  );
  return Promise.race([promise, timeoutRejectedPromise]);
};

export const mkTimeoutPromise = (timeoutMessage, timeoutMillis) => new Promise((resolve, reject) =>
  setTimeout(() => reject(timeoutMessage), timeoutMillis)
);

export const timedAsyncCall = (description, timeoutMillis, receiver, func, ...args) => {
  let timeoutReason = `Execution of ${description} timed out after ${timeoutMillis} milliseconds.`;
  let timeoutPromise = mkTimeoutPromise(timeoutReason, timeoutMillis);
  let promise = func.apply(receiver, args);
  return Promise.race([promise, timeoutPromise]);
};

export const mkTimeoutProxy = function(proxiedObj, timeoutMillis) {
  const handler = {
    get(target, propertyName, receiver) {
      const propertyValue = Reflect.get(target, propertyName, receiver);
      if (typeof propertyValue !== 'function')
        return propertyValue;
      let func = propertyValue;
      return function (...args) {
        console.log(`attempting to call ${propertyName} with timeout of ${timeoutMillis}`, args);
        // return func.apply(this, args);
        // TODO. How is 'this' resolved here? Should it be the receiver?
        return timedAsyncCall(propertyName, timeoutMillis, this, func, ...args);
      }
    }
  };
  return new Proxy(proxiedObj, handler);
};



