/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

/**
 * @module GameMockApi
 */

// NOTE. This module is no longer being maintained.

/*
 * Note. The data structures in this module all work on json objects,
 * and not on UI domain objects. The json objects are those that go
 * on the wire, or objects supporting those that go on the wire.
 */
import GameMockApiImpl from './GameMockApiImpl';
import {stringify} from "../../base/util/Logger";
import {happyResponsePromise} from "../../base/util/HttpUtil";

class GameMockApi {
  constructor(serverApiUrl) {
    // Associative array of game id => game service object.
    this.impls = {};
    this.nextGameId = 0;
  }

  getImpl(gameId) {
    let s = this.impls[gameId];
    if (s === undefined)
      throw `gameId: ${gameId} does not exists`;
    return s;
  }

  // Begin API.

  startGame(gameParams, initPieces, userId) {
    this.nextGameId += 1;
    let gameId = this.nextGameId;
    let impl = new GameMockApiImpl(gameId, gameParams, initPieces,  userId);
    this.impls[gameId] = impl;
    let gameDto = impl.gameDto;
    return happyResponsePromise(gameDto);
  }

  selectFirstPlayer(gameId) {
    let impl = this.getImpl(gameId);
    return happyResponsePromise(impl.selectFirstPlayer());
  }

  swap(gameId, piece) {
    let impl = this.getImpl(gameId);
    const swapResult = impl.swap(piece);
    return happyResponsePromise(swapResult);
  }

  commitPlay(gameId, playPieces) {
    let impl = this.getImpl(gameId);
    return happyResponsePromise(impl.commitPlay(playPieces));
  }

  getMachinePlay(gameId) {
    let impl = this.getImpl(gameId);
    const machinePlay = impl.getMachinePlay();
    return happyResponsePromise(machinePlay);
  }

  close(gameId) {
    return happyResponsePromise({});
  }

  suspend(gameId) {
    return happyResponsePromise({});
  }

  resume(gameId) {
    // TODO. Implement resume.
    return happyResponsePromise({});
  }

  cancel(gameId) {
    return happyResponsePromise({});
  }

  getUserGameSettings(userId) {
    // TODO. Implement getUserGamePreferences.
  }

  saveUserGameSettings(userId, settings) {
    // TODO. Implement setUserGamePreferences.
  }

  getUnfinishedUserGames(userId) {
    // TODO. Implement getUnfinishedUserGames.
  }

  getUserGames(userId, fromEpochSeconds, toEpochSeconds, maxGames) {
    // TODO. Implement.
  }

  /**
   * Used in tests.
   */
  filledPositions(gameId) {
    let impl = this.getImpl(gameId);
    return happyResponsePromise({
      positions: impl.filledPositions()
    });
  }

  /**
   * Used in tests.
   */
  getMachineTray(gameId) { // For testing.
    let impl = this.getImpl(gameId);
    return happyResponsePromise({
      tray: impl.machineTray
    });
  }

  // End API.

  // Auxiliary functions - TODO. should be generic in a super-class.
  // TODO. Remove from MockGameService.

  posFilled(gameId, pos) {
    let impl = this.getImpl(gameId);
    return happyResponsePromise(impl.posFilled(pos));
  }

  posEmpty(gameId, pos) {
    let impl = this.getImpl(gameId);
    return happyResponsePromise(impl.posEmpty(pos));
  }
}

export default GameMockApi;



