/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {mkGameTester} from "./testbase1";
import GameService from "../game/service/GameService";
import {errorTags} from "../game/domain/GameErrors";

let loginEvidence = null; // No login for guest.
let gameService = new GameService(loginEvidence);
let gameTester = mkGameTester(gameService);

test('start guest game and make user and machine plays', async () => {
    let game = await gameTester.startGame().unwrap;
    await gameTester.commitPlay(game.gameId).unwrap;
    await gameTester.machinePlay(game.gameId).unwrap;
    await gameTester.closeGame(game.gameId).unwrap;
});

test('cancel game', async () => {
  let game = await gameTester.startGame().unwrap;
  const gameId = game.gameId;
  await gameTester.commitPlay(game.gameId).unwrap;

  await gameService.cancel(gameId).unwrap;
  let result = await gameService.getFullGame(gameId).unwrap;
  expect(result.ok).toBe(false);
  expect(result.data.tag).toBe(errorTags.missingGame);
});

test('resign game', async () => {
  let game = await gameTester.startGame().unwrap;
  const gameId = game.gameId;
  await gameTester.commitPlay(game.gameId).unwrap;

  await gameService.resign(gameId).unwrap;
  let result = await gameService.getFullGame(gameId).unwrap;
  expect(result.ok).toBe(false);
  expect(result.data.tag).toBe(errorTags.missingGame);
});









