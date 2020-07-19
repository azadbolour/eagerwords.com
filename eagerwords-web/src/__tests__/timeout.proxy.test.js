/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import {mkTimeoutProxy} from '../base/util/MiscUtil';
test('timeout proxy', async (done) => {
  let value = 100;
  let obj250Waiter = {
    sumAfter250Millis: function(arg1, arg2) {
      return new Promise((resolve, reject) =>
        setTimeout(() => resolve(arg1 + arg2), 250)
      );
    },
  };

  let hurriedProxy = mkTimeoutProxy(obj250Waiter, 150);
  let shouldTimeout = hurriedProxy.sumAfter250Millis(1, 100);
  await shouldTimeout
    .then(val => done(`unexpected resolve`))
    .catch(reason => {console.log(reason)});

  let relaxedProxy = mkTimeoutProxy(obj250Waiter, 500);
  let shouldNotTimeout = relaxedProxy.sumAfter250Millis(1, 100);
  await shouldNotTimeout
    .then(val => {console.log(`sum: ${val}`); expect(val).toBe(1 + 100)})
    .catch(reason => {console.log(reason); done(`unexpected reject`)});

  done();
});