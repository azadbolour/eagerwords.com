/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {errorResult, valueResult, mappedValueResult} from '../base/domain/Result';
import {valueVow, errorVow} from '../base/domain/Vow';
import {stringify} from "../base/util/Logger";

const expandError = (error) => `expanded ${stringify(error)}`;
const times = (multiplier) => (value) => multiplier * value;
const plus = (addend) => value => value + addend;

const checkTruth = (truth) => expect(truth).toBe(true);
const checkEq = (actual, expected) => expect(actual).toStrictEqual(expected);

test('test result', () => {
  // Test mapValue.
  let value = 10;
  let result = valueResult(value);
  result = result.mapValue(times(10));
  expect(result.data).toBe(10 * value);

  // Test mapError.
  let result1 = result.mapError(expandError);
  checkEq(result1.unwrap, result.unwrap);

  // Test flatMap to value.
  let result2 = result1.flatMap(mappedValueResult(times(100)));
  checkEq(result2.data, 10000);
  checkTruth(result2.isValue);

  // Test flatMap to error.
  let result3 = result2.flatMap(() => errorResult('error'));
  checkEq(result3.data, 'error');
  checkTruth(result3.isError);

  // Test mapValue on error.
  let result4 = result3.mapValue(plus(300));
  checkEq(result4.unwrap, result3.unwrap);

  // Test flatMap to value on error.
  let result5 = result3.flatMap(mappedValueResult(times(100)));
  checkEq(result5.unwrap, result3.unwrap);

  // Test map error on error produced by flatMap.
  let result6 = result3.mapError(expandError);
  checkTruth(result6.isError);
  checkTruth(result6.data.startsWith('expanded'));
});

test('test construct vow', async (done) => {
  const value = 10;
  let vow = valueVow(value);
  vow.then(result => {
    checkTruth(result.isValue);
    checkEq(result.data, value);
    done();
  });
});

test('test map vow', async (done) => {
  const value = 10;
  let vow = valueVow(value);
  let mappedVow = vow.mapValue(times(10));
  mappedVow.then(result => {
    checkTruth(result.isValue);
    checkEq(result.data, times(10)(value));
    done();
  });
});

test('test map error vow', async (done) => {
  const error = 'error';
  let vow = errorVow(error);
  let mappedVow = vow.mapValue(times(10));
  mappedVow.then(result => {
    checkTruth(result.isError);
    checkEq(result.data, error);
    done();
  });
});

test('test mapError vow', async (done) => {
  const error = 'error';
  let vow = errorVow(error);
  let mappedVow = vow.mapError(expandError);
  mappedVow.then(result => {
    checkTruth(result.isError);
    checkTruth(result.data.startsWith('expanded'));
    done();
  });
});

test('test flatMap vow', async (done) => {
  const value = 10;
  let vow = valueVow(value);
  let mappedVow = vow.flatMap(mappedValueResult(times(100)));
  mappedVow.then(result => {
    checkTruth(result.isValue);
    checkEq(result.data, times(100)(value));
    done();
  });
});

test('test flatMap error vow', async (done) => {
  const error = 'error';
  let vow = errorVow(error);
  let mappedVow = vow.flatMap(mappedValueResult(times(100)));
  mappedVow.then(result => {
    checkTruth(result.isError);
    checkEq(result.data, error);
    done();
  });
});
