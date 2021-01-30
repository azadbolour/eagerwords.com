
// TODO. Not used yet. Copy to Vow.js. Also copy Result1.js to Result.js.

import {valueResult, errorResult, Result, isResult, mappedValueResult} from './Result';
import {stringify, objectInfo} from "../util/Logger";

// let checkVow = function(message, obj) {
//   let u = obj.unwrap;
//   let hasPromise = u instanceof Promise;
//   let isValue = obj.isValue;
//   let typ = (isValue === undefined || isValue === null) ? "vow" : "result"
//   console.log(`${message} - is ${typ}, has promise: ${hasPromise}`);
// };

// let objectInfo = (message, varName, obj) => {
//   console.log(`${message} - var: ${varName}`);
//   console.log(`  properties: ${Object.getOwnPropertyNames(obj).slice(0, 10)}`);
//   console.log(`  value: ${stringify(obj).replace(/[\r\n]/g, "").substr(0, 120)} ...`);
//   if (obj.data !== undefined)
//     console.log(`  ${varName}.data: ${stringify(obj.data).replace(/[\r\n]/g, "").substr(0, 50)} ...`);
//   return obj;
// };

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

    // This is confusing since the function needs the promise.
    // Deprecated. TODO. Use mapResult or passResult.
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

    debugBrief: (location) => {
      let mappedPromise = promise
        .then(result => {
          return objectInfo(location, "result", result);
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

    /**
     * Monadic flatMap for Vow. Given a function from the substrate to a vow,
     * return a vow that feeds the value of the current vow when available into
     * that function to get a new vow.
     *
     * @param func: T => Vow[S], i.e., T => Vow[Promise[Result[S]]]
     */
    flatMap /* Vow[T] => Vow[S] */: (func) => {
      let flatMappedPromise /* Promise[Result[S]] */ = promise
        .then(result /* Result[T] */ => {
          if (!isResult(result)) {
            console.log(`type error: Vow yields a non-Result`);
            objectInfo("Vow.flatMap", "result", result);
            // throw new Error("not a result");
            // TODO. Fatal error. Or will it fixed with typescript?
          }

          /*
           * If the result is in error, there is nothing to do; just use the result.
           * Otherwise, compute the resulting vow, and use its embedded promise.
           */
          let chainedPromise = (result.ok) ? func(result.data).unwrap : Promise.resolve(result);

          // Return the new promise so it can be chained with this vow's promise via the Promise machinery.
          return chainedPromise;
        })
        .catch(reason => {
          return errorResultCatcher(reason)
        });
      // console.log(`returning from Vow flatMap: do we have a promise? ${flatMappedPromise instanceof Promise}`);
      return Vow(flatMappedPromise);
      // .debugBrief("Vow.flatMap.return");
    },

    // flatMap: (func) => {
    //   let flatMappedPromise = promise
    //     .then(result => {return result.flatMap(func)})
    //     .catch(reason => {return errorResultCatcher(reason)});
    //   return Vow(flatMappedPromise);
    // },
    //

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
export const resultVow = (result => Vow(Promise.resolve(result)));

export const mappedValueVow = (mapper) => (value) => Vow(Promise.resolve(valueResult(value).mapValue(mapper)));
