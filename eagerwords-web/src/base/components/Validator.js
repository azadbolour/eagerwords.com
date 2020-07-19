/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

const naturalRangeValidator = function(value, minMax) {
  if (!naturalRangeValidator(value))
    return false;
  const {min, max} = minMax;
  return value >= min && value <= max;
};

const naturalValidator = function(value) {
  return /^(\d+)$/.test(value);
};

const requiredValidator = function(value) {
  return value && value.trim !== '';
};

const validators = {
  required: requiredValidator,
  natural: naturalValidator,
  naturalRange: naturalRangeValidator
};

// For now break after the first validation error.
export const validate = function(value, rules) {
  for (let rule in rules) {
    const {ruleName, params, message} = rule;
    const validator = validators[ruleName];
    if (validator === undefined)
      continue;
    if (!validator(value, params)) {
      return {valid: false, message}
    }
  }
  return {valid: true};
};

