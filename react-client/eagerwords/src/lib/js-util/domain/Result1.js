/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

// TODO. Not used yet. Copy to Result.js at the same time at copying Vow1.js to
// Vow.js.

/**
 * The Result monad - an implementation of the Try in javascript.
 *
 * @param ok Whether it has a real value or an error.
 * @param data The real value f the error.
 */
import {stringify, objectInfo} from "../util/Logger";

let checkVow = function(message, obj) {
  let u = obj.unwrap;
  let hasPromise = u instanceof Promise;
  let isValue = obj.isValue;
  let typ = (isValue === undefined || isValue === null) ? "vow" : "result"
  console.log(`${message} - is ${typ}, has promise: ${hasPromise}`);
};

// TODO. URGENT. Check that ok is a boolean, and data is not undefined.
// typeof ok === "boolean"
export const Result = function({ok, data}) {
  return {
    get unwrap() {return {ok, data}},
    get ok() {return ok},
    get data() {return data},
    get isValue() {return ok},
    get isError() {return !ok},

    passValue: (func) => {
      if (ok) func(data);
      return this;
    },
    mapValue: (func) => Result(ok ? {ok, data: func(data)} : {ok, data}),
    mapError: (func) => Result(ok ? {ok, data} : {ok, data: func(data)}),

    /**
     * Monadic flatMap for Result. Apply the function to the substrate to get
     * a new result if substrate exists (i.e., ok), or just return this if !ok.
     *
     * @param func: T => Result[S]
     */
    flatMap: (func) => ok ? func(data) : Result({ok, data}),
    flatMapError: (func) => ok ? Result({ok, data}) : func(data),
    transform: (func, errFunc) => ok ? this.mapValue(func) : this.mapError(errFunc)
  }
};

export const resultMapValue = (func) => (result) => {
  // console.log(`result: ${stringify(result)}`);
  return result.mapValue(func);
};

export const valueResult = (value) => Result({ok: true, data: value});
export const errorResult = (error) => Result({ok: false, data: error});
export const mappedValueResult = (mapper) => (value) => valueResult(value).mapValue(mapper);

// This is a basic check, a hack really, for initial expedience.
export const isResult = (obj) =>
  obj.hasOwnProperty('ok') && obj.hasOwnProperty('data');
