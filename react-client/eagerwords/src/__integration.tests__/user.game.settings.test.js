/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {defaultGamePlayParams} from "../game/domain/GamePlayParams";
import {defaultGameLookAndFeelParams, squareSize} from "../game/domain/GameLookAndFeelParams";
import {DeviceTypes} from "lib/js-util/index";
import GameService from "../game/service/GameService";
import {stringify} from 'lib/js-util/index';
import {HttpUtil} from "lib/js-util/index";
import {authTester} from "./testbase1";
import {mkUserGameSettings} from "../game/domain/UserGameSettings";

let {errorText} = HttpUtil;
let {deviceTypes} = DeviceTypes;

// TODO. Better use game event handler rather than game service - better error handling.

const playSettings = {...defaultGamePlayParams, dimension: 17};
const lookAndFeelSettings = {...defaultGameLookAndFeelParams, squareSize: squareSize.large};

let userGameSettings = mkUserGameSettings(playSettings, lookAndFeelSettings);

const checkAndGetOkJson = (response, actionName) => {
  if (!response.ok)
    console.log(`error in ${actionName}: ${errorText(response)}`);
  expect(response.ok).toBe(true);
  return response.json;
};

test('start game and store and retrieve game settings', async () => {
    let response, json;

    let loginEvidence = await authTester.mockSignUpOrLogin();
    let gameService = new GameService(loginEvidence);

    let saveVow = gameService.saveUserGameSettings(userGameSettings);
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
    expect(settings).toMatchObject(userGameSettings);

    // let settings1 = {...userGameSettings, preferredDevice: deviceTypes.touch};
    let lookAndFeelSettings1 = {...lookAndFeelSettings, preferredDevice: deviceTypes.touch};
    let settings1 = mkUserGameSettings(playSettings, lookAndFeelSettings1);

    saveVow = gameService.saveUserGameSettings(settings1);
    await saveVow.unwrap;

    getVow = gameService.getUserGameSettings();
    getResult = await getVow.unwrap;
    console.log(`getResult: ${stringify(getResult)}`);
    expect(getResult.ok).toBe(true);
    settings = getResult.data;
    expect(settings.lookAndFeelSettings.preferredDevice).toBe(deviceTypes.touch);

});











