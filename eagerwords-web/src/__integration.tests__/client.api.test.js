/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {authTester, mkGameTester} from "./testbase1";
import BaseService from "../base/service/BaseService";
import {errorText} from "../base/util/HttpUtil";
import GameService from "../game/service/GameService";
import {playPiecesWord} from "../game/domain/PlayPiece";
import {stringify} from "../base/util/Logger";
import {resultErrorMessage} from "../base/domain/BaseErrors";
import {errorTags} from "../game/domain/GameErrors";
import {RUN_STATE} from "../game/domain/Game";

test('start game and make user and machine plays', async done => {
  try {
    let baseService = new BaseService();
    let response;

    response = await baseService.handShake();
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

    await gameTester.suspendGame(gameId);
    let suspendedGame = await gameTester.getFullGame(gameId);

    let ignored = await gameTester.resumeGame(gameId);

    let resumedGame = await gameTester.getFullGame(gameId);
    let plays = resumedGame.plays;
    let wordsPlayed = plays.wordsPlayed;
    committedPlayPieces.forEach(playPiece => {
      let piece = playPiece.piece;
      let point = playPiece.point;
      expect(resumedGame.board.getPlayPiece(point).piece.value).toBe(piece.value);
    });
    expect(wordsPlayed.map(el => el.word)[0]).toBe(playPiecesWord(committedPlayPieces));
    expect(resumedGame.running()).toBe(true);

    done();
  } catch(reason) {
    // Rejections will throw and get caught here.
    done.fail(reason)
  }
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

// test.skip
test('cancel game', async done => {
  const {gameService, gameTester, game} = await initTest();
  const gameId = game.gameId;

  await gameService.cancel(gameId).unwrap;
  let result = await gameService.getFullGame(gameId).unwrap;
  expect(result.ok).toBe(false);
  expect(result.data.tag).toBe(errorTags.missingGame);
  done();
});

test('resign game', async done => {
  const {gameService, gameTester, game} = await initTest();
  const gameId = game.gameId;

  await gameService.resign(gameId).unwrap;
  let result = await gameService.getFullGame(gameId).unwrap;
  expect(result.ok).toBe(true);
  let resignedGame = result.data;
  expect(resignedGame.runState).toBe(RUN_STATE.FINISHED);
  done();
});
