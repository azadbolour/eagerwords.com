/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

// import {restManager} from '../jsutil/util/RestManager';
import {RestManager} from 'lib/js-util/index';
// TODO. URGENT. Remove dependency on params. Use envvars.
// import {defaultHostName, defaultServerUrl, defaultTestServerPort, mkServerUrl} from '../envvars';
import {stringify} from 'lib/js-util/index';
import {envvars} from 'lib/js-util/index';

let {defaultHostName, defaultTestServerPort, mkServerUrl} = envvars;
let {restManager} = RestManager;

test('bad server url', () => {
  let body = JSON.stringify({noMatter: ''});
  let request = restManager.mkPostRequest(body);
  let promise = restManager.send(request, 'http://localhost:1234567890', '/game/doesnoexist');
  promise.then(response => {
    console.log(`response - ${stringify(response)}`);
    expect(response.ok).toBe(false);
    expect(response.status).toBe(500); // We are overloading 500 to indicate rejections.
  });
});

test('non-existent server', () => {
  let body = JSON.stringify({noMatter: ''});
  let request = restManager.mkPostRequest(body);
  let promise = restManager.send(request, 'http://localhost:1111', '/game/doesnoexist');
  promise.then(response => {
    console.log(`response - ${stringify(response)}`);
    expect(response.ok).toBe(false);
    expect(response.status).toBe(500); // We are overloading 500 to indicate rejections.
  });
});

test('not found - send request to existing server - non-existent path', () => {
  let body = JSON.stringify({noMatter: ''});
  let request = restManager.mkPostRequest(body);
  let serverUrl = mkServerUrl('http:', defaultHostName, defaultTestServerPort);
  let promise = restManager.send(request, serverUrl, '/game/doesnoexist');
  promise.then(response => {
    // console.log(`response - ${stringify(response)}`);
    expect(response.ok).toBe(false);
    expect(response.status).toBe(404);
  });
});

test('invalid request - body does not type check', () => {
  let body = JSON.stringify({wrong: ''});
  let request = restManager.mkPostRequest(body);
  let serverUrl = mkServerUrl('http:', defaultHostName, defaultTestServerPort);
  console.log(`server url: ${serverUrl}`)
  let promise = restManager.send(request, serverUrl, '/game/game');
  promise.then(response => {
    // console.log(`response - ${stringify(response)}`);
    expect(response.ok).toBe(false);
    expect(response.status).toBe(400);
  });
});
