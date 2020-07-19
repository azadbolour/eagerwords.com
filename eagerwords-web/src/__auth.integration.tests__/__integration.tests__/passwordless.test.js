/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

// TODO. Make sure the server uses mock email service.
// Use handshake to tell the server which email service to use.

import {baseApis, apiImpl} from "../api/ApiUtil";
import {apiTypes} from '../envvars';
import { v4 as uuidv4 } from 'uuid';
import {stringify} from "../util/Logger";

// TODO. Might as well forget about integration tests for passwordless authentication,
// since can't easily get a hold of the emailed token.

// Unfortunately we don't have the machinery to get the token.
// So our options for integration testing are limited for now.
// We are stuck with manual testing.
// TODO. For testing we need a mode in which the token is returned with the clientId.
// But for now we won't complicate the code base any more.
test('initSignUp', done => {
  let nickname = 'bob';
  let api = baseApis[apiTypes.client];
  let domain = uuidv4();
  let email = `bob@${domain}`;
  console.log('attempting initSignUp');
  api.initSignUp(email, nickname, 2000).then(response => {
    // console.log(`${stringify(response)}`);
    expect(response.ok).toBe(true);
    let clientId = response.json.clientId;
    console.log(`clientId: ${clientId}`);
    done();
  })
});