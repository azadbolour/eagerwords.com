/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/** @module GameClientApi */

import {StartGameRequestConverter} from "./GameConverters";
import {restManager} from '../../base/util/RestManager';
import {stringify} from "../../base/util/Logger";
import {getServerUrlBase} from "../../envvars";

// TODO. Logging for requests and responses.

class GameClientApi {
  constructor(serverUrl) {
    this.serverUrl = serverUrl;
  }

  // TODO. URGENT. startGame(loginEvidence: Option, gameParams, iniPieces)
  // None means guest.

  /**
   * Start a new game.
   *
   * @param loginEvidence - Evidence that the user has logged in. Null for guests.
   * @param gameParams - Basic properties of the game.
   * @param initPieces Initial board pieces - for testing.
   */
  startGame(loginEvidence, gameParams, initPieces) {
    let startGameRequest = {
      loginEvidence,
      gameParams,
      initPieces
    };
    let body = JSON.stringify(startGameRequest);
    let request = restManager.mkPostRequest(body);
    let promise = restManager.send(request, this.serverUrl, '/game/game');
    return promise;
  }

  // TODO. URGENT. commitPlay(loginEvidence: Option, gameId, playPieces)

  /**
   * Commit a user play.
   *
   * @param loginEvidence - Evidence that the user is logged in.
   * @param gameId The game's unique id.
   * @param playPieces List of individual piece plays.
   */
  commitPlay(loginEvidence, gameId, playPieces) {
    let commitPlayRequest = {
      loginEvidence,
      playPieces
    };
    let body = JSON.stringify(commitPlayRequest);
    let request = restManager.mkPostRequest(body);
    let restPath = `/game/commit-play/${gameId}`;
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }

  // TODO. URGENT. getMachinePlay(loginEvidence, gameId)

  getMachinePlay(loginEvidence, gameId) {
    let body = JSON.stringify(loginEvidence);
    let request = restManager.mkPostRequest(body);
    let restPath = `/game/machine-play/${gameId}`;
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }

  // TODO. URGENT. swap(loginEvidence, gameId, piece)

  swap(loginEvidence, gameId, piece) {
    let swapPieceRequest = {
      loginEvidence,
      piece
    };
    let body = JSON.stringify(swapPieceRequest);
    let request = restManager.mkPostRequest(body);
    let requestPath = '/game/swap-piece/' + gameId;
    let promise = restManager.send(request, this.serverUrl, requestPath);
    return promise;
  }

  getFullGame(loginEvidence, gameId) {
    return this.postAuthorizedByGameId(loginEvidence, gameId, '/game/fullgame');
  }

  close(loginEvidence, gameId) {
    return this.postAuthorizedByGameId(loginEvidence, gameId, '/game/close');
  }

  suspend(loginEvidence, gameId) {
    return this.postAuthorizedByGameId(loginEvidence, gameId, '/game/suspend');
  }

  resume(loginEvidence, gameId) {
    return this.postAuthorizedByGameId(loginEvidence, gameId, '/game/resume');
  }

  cancel(loginEvidence, gameId) {
    return this.postAuthorizedByGameId(loginEvidence, gameId, '/game/cancel');
  }

  resign(loginEvidence, gameId) {
    return this.postAuthorizedByGameId(loginEvidence, gameId, '/game/resign');
  }

  /**
   * @param loginEvidence - Evidence of login - cannot be null - no settings for guest.
   */
  getUserGameSettings(loginEvidence) {
    return this.postAuthorized(loginEvidence, '/game/settings/get');
  }

  saveUserGameSettings(loginEvidence, settings) {
    let saveRequest = {
      loginEvidence,
      settings
    };
    let body = JSON.stringify(saveRequest);
    let request = restManager.mkPostRequest(body);
    let restPath = '/game/settings/save';
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }

  getUnfinishedUserGames(loginEvidence) {
    return this.postAuthorized(loginEvidence, '/game/unfinished');
  }

  getUserGames(loginEvidence, fromEpochSecond, toEpochSecond, maxGames) {
    let payload = {
      loginEvidence,
      fromEpochSecond,
      toEpochSecond,
      maxGames
    };
    let body = JSON.stringify(payload);
    let request = restManager.mkPostRequest(body);
    let restPath = '/game/all';
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }

  unregisterUser(loginEvidence) {
    let body = JSON.stringify(loginEvidence);
    let request = restManager.mkPostRequest(body);
    let restPath = "/admin/unregister";
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }

  postByIdOnly(id, pathPrefix) {
    let req = restManager.mkEmptyPostRequest();
    let restPath = `${pathPrefix}/${id}`;
    let promise = restManager.send(req, this.serverUrl, restPath);
    return promise;
  }

  postAuthorizedByGameId(loginEvidence, gameId, pathPrefix) {
    let body = JSON.stringify(loginEvidence);
    let request = restManager.mkPostRequest(body);
    let restPath = `${pathPrefix}/${gameId}`;
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }

  postAuthorized(loginEvidence, restPath) {
    let body = JSON.stringify(loginEvidence);
    let request = restManager.mkPostRequest(body);
    let promise = restManager.send(request, this.serverUrl, restPath);
    return promise;
  }

  getByIdOnly(id, pathPrefix) {
    let req = restManager.mkGetRequest();
    let restPath = `${pathPrefix}/${id}`;
    let promise = restManager.send(req, this.serverUrl, restPath);
    return promise;
  }

}


export default GameClientApi;
