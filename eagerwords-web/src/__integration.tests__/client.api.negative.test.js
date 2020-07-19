/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {restManager} from '../base/util/RestManager';
// TODO. URGENT. Remove dependency on params. Use envvars.
import {defaultServerUrl} from '../envvars';
import {stringify} from '../base/util/Logger';

test('bad server url', done => {
  let body = JSON.stringify({noMatter: ''});
  let request = restManager.mkPostRequest(body);
  let promise = restManager.send(request, 'http://localhost:1234567890', '/game/doesnoexist');
  promise.then(response => {
    console.log(`response - ${stringify(response)}`);
    expect(response.ok).toBe(false);
    expect(response.status).toBe(500); // We are overloading 500 to indicate rejections.
    done()
  });
});

test('non-existent server', done => {
  let body = JSON.stringify({noMatter: ''});
  let request = restManager.mkPostRequest(body);
  let promise = restManager.send(request, 'http://localhost:1111', '/game/doesnoexist');
  promise.then(response => {
    console.log(`response - ${stringify(response)}`);
    expect(response.ok).toBe(false);
    expect(response.status).toBe(500); // We are overloading 500 to indicate rejections.
    done()
  });
});


test('not found - send request to existing server - non-existent path', done => {
  let body = JSON.stringify({noMatter: ''});
  let request = restManager.mkPostRequest(body);
  let promise = restManager.send(request, defaultServerUrl, '/game/doesnoexist');
  promise.then(response => {
    // console.log(`response - ${stringify(response)}`);
    expect(response.ok).toBe(false);
    expect(response.status).toBe(404);
    done()
  });
});

test('invalid request - body does not type check', done => {
  let body = JSON.stringify({wrong: ''});
  let request = restManager.mkPostRequest(body);
  let promise = restManager.send(request, defaultServerUrl, '/game/game');
  promise.then(response => {
    // console.log(`response - ${stringify(response)}`);
    expect(response.ok).toBe(false);
    expect(response.status).toBe(400);
    done()
  });
});
