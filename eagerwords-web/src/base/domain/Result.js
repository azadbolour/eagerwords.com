/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

/**
 * The Result monad - an implementation of the Try in javascript.
 *
 * @param ok Whether it has a real value or an error.
 * @param data The real value f the error.
 */
import {stringify} from "../util/Logger";

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
    flatMap: (func) => ok ? func(data) : Result({ok, data}),
    flatMapError: (func) => ok ? Result({ok, data}) : func(data),
    transform: (func, errFunc) => ok ? this.mapValue(func) : this.mapError(errFunc)
  }
};

export const resultMapValue = (func) => (result) => {
  console.log(`result: ${stringify(result)}`);
  return result.mapValue(func);
};

export const valueResult = (value) => Result({ok: true, data: value});
export const errorResult = (error) => Result({ok: false, data: error});
export const mappedValueResult = (mapper) => (value) => valueResult(value).mapValue(mapper);

// This is a basic check, a hack really, for initial expedience.
export const isResult = (obj) =>
  obj.hasOwnProperty('ok') && obj.hasOwnProperty('data');