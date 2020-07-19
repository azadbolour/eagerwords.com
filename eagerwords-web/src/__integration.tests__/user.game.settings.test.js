/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import {defaultGameSettings} from "../game/domain/GameSettings";
import {deviceTypes} from "../base/domain/DeviceTypes";
import GameService from "../game/service/GameService";
import BaseService from "../base/service/BaseService";
import {stringify} from "../base/util/Logger";
import {errorText} from "../base/util/HttpUtil";
import {isEmpty} from "../base/util/MiscUtil";
import {authTester} from "./testbase1";

// TODO. Better use game event handler rather than game service - better error handling.

let userSettings = {...defaultGameSettings, dimension: 17, squarePixels: 100};

const checkAndGetOkJson = (response, actionName) => {
  if (!response.ok)
    console.log(`error in ${actionName}: ${errorText(response)}`);
  expect(response.ok).toBe(true);
  return response.json;
};

test('start game and store and retrieve game settings', async (done) => {
  try {
    let response, json;

    let loginEvidence = await authTester.mockSignUpOrLogin();
    let gameService = new GameService(loginEvidence);

    let saveVow = gameService.saveUserGameSettings(userSettings);
    let saveResult = await saveVow.unwrap;
    expect(saveResult.ok).toBe(true);

    // let unit = checkAndGetOkJson(response, "saveUserGameSettings");
    // console.log(`unit json: ${stringify(unit)}`); // This prints {}.
    // json = Object.assign({}, unit);
    // expect(isEmpty(json)).toBe(true); // This succeeds.
    // expect(unit).toBe({}); // This fails. But the object is in fact empty.

    let getVow = gameService.getUserGameSettings();
    let getResult = await getVow.unwrap;
    console.log(`getResult: ${stringify(getResult)}`);
    expect(getResult.ok).toBe(true);
    let settings = getResult.data;
    expect({...settings}).toMatchObject(userSettings);

    let settings1 = {...userSettings, preferredDevice: deviceTypes.touch};

    saveVow = gameService.saveUserGameSettings(settings1);
    await saveVow.unwrap;

    getVow = gameService.getUserGameSettings();
    getResult = await getVow.unwrap;
    console.log(`getResult: ${stringify(getResult)}`);
    expect(getResult.ok).toBe(true);
    settings = getResult.data;
    expect(settings.preferredDevice).toBe(deviceTypes.touch);

    done();
  } catch(reason) {
    done.fail(reason);
  }
});











