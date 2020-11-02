/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {authTester, mkGameTester} from "./testbase1";
import {HttpUtil} from "lib/js-util/index";
import GameService from "../game/service/GameService";
import {stringify} from 'lib/js-util/index';
import {BaseErrors} from "lib/js-util/index";
import {errorTags} from "../game/domain/GameErrors";
import {RUN_STATE} from "../game/domain/Game";
import {mkClientApi} from "../game/api/ApiUtil";

let {errorText} = HttpUtil;
let {resultErrorMessage} = BaseErrors;

test('start game and make user and machine plays', async () => {
  let response;
  let clientApi = mkClientApi();
  response = await clientApi.handShake();
  if (!response.ok)
    console.log(`error in handshake with server: ${errorText(response)}`);
  expect(response.ok).toBe(true);

  // Just ascertain that we are working with a Vow.
  let loginVow = authTester.mockSignUpOrLogin()
    .passResult(result => console.log(`vow result: ${stringify(result)}`));
  let loginEvidence = await loginVow.unwrap;

  let gameService = new GameService(loginEvidence);
  let gameTester = mkGameTester(gameService);

  let game = await gameTester.startGame();
  let gameId = game.gameId;
  console.log(`gameId: ${gameId}`);
  let committedPlayPieces = await gameTester.commitPlay(gameId);
  await gameTester.machinePlay(gameId);
  await gameTester.closeGame(gameId);

  let board = game.board;

  let userGamesVow = gameService.getUserGames(0, 0, 5).unwrap;
  let userGamesResult = await userGamesVow;
  if (!userGamesResult.ok)
    console.log(`error in getting user games: ${resultErrorMessage(userGamesResult)}`);
  expect(userGamesResult.ok).toBe(true);
  let gamesInfo = userGamesResult.data;
  expect(gamesInfo.length).toBeGreaterThan(0);
});

const initTest = async function() {
  let loginVow = authTester.mockSignUpOrLogin()
    .passResult(result => console.log(`vow result: ${stringify(result)}`));
  let loginEvidence = await loginVow.unwrap;

  let gameService = new GameService(loginEvidence);
  let gameTester = mkGameTester(gameService);

  let game = await gameTester.startGame();
  let gameId = game.gameId;
  console.log(`gameId: ${gameId}`);
  let committedPlayPieces = await gameTester.commitPlay(gameId);
  await gameTester.machinePlay(gameId);
  return {gameService, gameTester, game};
};

// test.skip('cancel game', async () => {
test('cancel game', async () => {
  const {gameService, gameTester, game} = await initTest();
  const gameId = game.gameId;

  await gameService.cancel(gameId).unwrap;
  let result = await gameService.getFullGame(gameId).unwrap;
  expect(result.ok).toBe(false);
  expect(result.data.tag).toBe(errorTags.missingGame);
  // done();
});

test('resign game', async () => {
  const {gameService, gameTester, game} = await initTest();
  const gameId = game.gameId;

  await gameService.resign(gameId).unwrap;
  let result = await gameService.getFullGame(gameId).unwrap;
  expect(result.ok).toBe(true);
  let resignedGame = result.data;
  expect(resignedGame.runState).toBe(RUN_STATE.FINISHED);
});
