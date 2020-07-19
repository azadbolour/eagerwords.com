
import {valueResult, errorResult, Result} from './Result';
import {stringify} from "../util/Logger";

// TODO. URGENT. Use error constants from BaseErrors once integrated.
/**
 * Convert rejection reason to error results.
 */
const errorResultCatcher = (reason) => {
  console.error(reason);
  const error = {
    tag: 'Rejection',
    classifier: 'unrecoverable',
    message: `internal error: ${stringify(reason)}`
  };
  return errorResult(error);
};

const errorResultNotPromise = (notAPromise) => errorResult({
  tag: 'NotAPromise',
  classifier: 'unrecoverable',
  message: `type error: Vow received a non-promise: ${stringify(notAPromise)}`
});

// TODO. URGENT. Promise value of a Vow must be a Result: check by using Result's isResult.

/**
 * Monad for Promise[Result[T]].
 *
 * Catches are consumed and converted to errors.
 *
 * NOTE. All functions create new promises and new vows from the given promise.
 * They do not mess with the promise itself.
 *
 * Awaiting on the promise in a Vow function changes the promise and does not work
 * as expected.
 *
 * @param promiseArg
 */
export const Vow = function(promiseArg) {
  if (!(promiseArg instanceof Promise))
    return Vow(errorResultNotPromise());

  // Make sure vow swallows rejections, converting them to Result errors.
  const promise = promiseArg.catch(reason => {return errorResultCatcher(reason)});

  return {
    get unwrap() {return promise},

    // This is confusing since the function needs the promise,
    // vs Result or its constituents.
    // Refactor to mapValue once everything has been converted to Vow.
    then: (func) => {return Vow(promise.then(func))},

    mapResult: (func) => {
      let mappedPromise = promise
        .then(result => {return func(result)})
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(mappedPromise);
    },

    debugResult: (message) => {
      if (!message)
        message = '';
      let mappedPromise = promise
        .then(result => {
          console.log(`${message}`);
          console.log(`${stringify(result)}`);
          return result;
        })
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(mappedPromise);
    },

    passResult: (func) => {
      let mappedPromise = promise
        .then(result => {func(result); return result;})
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(mappedPromise);
    },

    mapValue: (func) => {
      let mappedPromise = promise
        .then(result => {return result.mapValue(func)})
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(mappedPromise);
    },

    passValue: (func) => {
      let mappedPromise = promise
        .then(result => result.passValue(func))
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(mappedPromise);
    },

    mapError: (func) => {
      let mappedPromise = promise
        .then(result => {return result.mapError(func)})
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(mappedPromise);
    },

    flatMap: (func) => {
      let flatMappedPromise = promise
        .then(result => {return result.flatMap(func)})
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(flatMappedPromise);
    },

    flatMapError: (func) => {
      let flatMappedPromise = promise
        .then(result => {return result.flatMapError(func)})
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(flatMappedPromise);
    },

    transform: (func, errFunc) => {
      let transformedPromise = promise
        .then(result => {return result.transform(func, errFunc)})
        .catch(reason => {return errorResultCatcher(reason)});
      return Vow(transformedPromise);
    }
  }
};

export const valueVow = (value) => Vow(Promise.resolve(valueResult(value)));
export const errorVow = (error) => Vow(Promise.resolve(errorResult(error)));

export const resolveVow = ({ok, data}) => Vow(Promise.resolve(Result({ok, data})));