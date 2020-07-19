/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import {gameResponseToResultPromiseMapper} from '../game/domain/GameErrors';
import {
  errorClassifiers,
  errorTags
} from "../base/domain/BaseErrors";
import {stringify} from "../base/util/Logger";

test('process happy response', async (done) => {
  console.log(`${stringify(errorClassifiers)}`);
  let response = {
    ok: true,
    json: {
      info: 'abc'
    }
  };
  let responsePromise = Promise.resolve(response);
  let resultPromise = gameResponseToResultPromiseMapper(responsePromise);
  let result = await resultPromise;
  expect(result.ok).toBe(true);
  expect(result.data.info).toBe('abc');
  done();
});

test('process bad request', async (done) => {
  let response = {
    ok: false,
    status: 400,
    statusText: 'Bad Request'
  };
  let responsePromise = Promise.resolve(response);
  let resultPromise = gameResponseToResultPromiseMapper(responsePromise);
  let result = await resultPromise;
  expect(result.ok).toBe(false);
  expect(result.data.tag).toBe(errorTags.badRequest);
  done();
});