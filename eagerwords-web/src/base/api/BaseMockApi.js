/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/**
 * @module GeneralMockApi
 */

import {happyResponsePromise} from "../util/HttpUtil";

let mockEulaAccepted = false;

class BaseMockApi {
  constructor(serverApiUrl) {
  }

  // Begin API.

  handShake() {
    let json = {
      serverType: "Mock",
      apiVersion: "1.0"
    };
    return happyResponsePromise(json);
  }

  saveUser(jsonUser) {
    mockEulaAccepted = jsonUser.eulaAccepted;
    // For now ignore.
    return happyResponsePromise({});
  }

  getUser(userId) {
    return happyResponsePromise({
      eulaAccepted: mockEulaAccepted
    });
  }
}

export default BaseMockApi;



