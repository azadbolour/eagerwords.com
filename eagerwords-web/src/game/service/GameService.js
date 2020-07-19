/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/**
 * @module GameService
 */

import {gameApis} from '../../game/api/ApiUtil';
import {GameConverter, GameParamsConverter, PieceConverter, SettingsConverter} from '../api/GameConverters';
import {convertHappyResponse} from "../../base/util/HttpUtil";
import {PlayPieceConverter, PointConverter} from "../api/GameConverters";
import {stringify} from "../../base/util/Logger";
import {getApiType} from "../../envvars";
import {gameResponseToResultPromiseMapper} from "../domain/GameErrors";
import {apiMockErrorAdapter} from "../../base/domain/ApiAdapters";
import {Vow} from "../../base/domain/Vow";

// NOTE. This module is in the process of incremental refactoring
// to use include timeouts in API calls, catch rejections, and convert
// http responses to a standard Result data structure.

// TODO. URGENT. Refactor the doc comments below based on the new Vow/Result monad.
// Some comments may have to move to the client API.

/**
 * OBSOLETE COMMENTS.
 *
 * Abstraction layer above the api to hide the api implementation,
 * and to convert from application data structures to api data structures.
 *
 * The API calls used by this module return an HTTP response data structure,
 * since that data structure includes fields needed to represent what happened
 * as a result of the request. The json and message fields of the response
 * data structure include converted data, as follows.
 *
 * If the response indicates success, the response json is replaced with
 * an application-level data structure representing the returned data from the
 * API call.
 *
 * If the response indicated failure, it is returned verbatim, to provide
 * maximum information about the error to the caller.
 *
 * Here is an example of a response in case of failure:
 *
 * {
 *   "json": {
 *   "tag": "InvalidWordError",
 *   "message": "'BG' not found in the dictionary",
 *   "languageCode": "en",
 *   "word": "BG"
 *   },
 *   "ok": false,
 *   "status": 422,
 *   "statusText": "Unprocessable Entity"
 * }
 *
 */

const serviceWrapper = function(mockErrors, api, func, ...args) {
  let promise = apiMockErrorAdapter(func.name, mockErrors)(api, func, ...args);
  let promise1 = promise.then(response => {
    // console.log(`response: ${stringify(response)}`);
    return response;
  });
  return Vow(gameResponseToResultPromiseMapper(promise1));
};

class GameService {
  constructor(maybeLoginEvidence) {
    this.loginEvidence = maybeLoginEvidence;
    this.api = gameApis[getApiType()];
  }

  /**
   * Start a new game.
   *
   * @param gameParams - Game settings + pointValues.
   * @param initPieces Initial board pieces - for testing.
   */
  start(gameParams, initPieces, mockErrors) {
    let api = this.api;
    const paramsJson = GameParamsConverter.toJson(gameParams);
    return serviceWrapper(mockErrors, api, api.startGame, this.loginEvidence, paramsJson, initPieces)
      .mapValue((gameDto) => GameConverter.startGameResponseToGame(gameDto, gameParams));
  }

  commitUserPlay(gameId, playPieces, mockErrors) {
    let api = this.api;

    let commitDataConverter = (commitData) => {
      let {gameMiniState, replacementPieces, deadPoints} = commitData;
      replacementPieces = replacementPieces.map(PieceConverter.fromJson);
      deadPoints = deadPoints.map(js => PointConverter.fromJson(js)); // TODO. Simplify.
      return {gameMiniState, replacementPieces, deadPoints};
    };

    let jsonPlayPieces = playPieces.map(PlayPieceConverter.toJson);
    return serviceWrapper(mockErrors, api, api.commitPlay, this.loginEvidence, gameId, jsonPlayPieces)
      .mapValue(commitDataConverter);
  }

  getMachinePlay(gameId, mockErrors) {
    let api = this.api;

    let playDataConverter = (playData) => {
      let {gameMiniState, playedPieces, deadPoints} = playData;
      playedPieces = playedPieces.map(playPiece => PlayPieceConverter.fromJson(playPiece));
      deadPoints = deadPoints.map(js => PointConverter.fromJson(js));
      return {gameMiniState, playedPieces, deadPoints};
    };

    return serviceWrapper(mockErrors, api, api.getMachinePlay, this.loginEvidence, gameId)
      .mapValue(playDataConverter);
  }
  
  swap(gameId, pc, mockErrors) {
    let api = this.api;

    let swapDataConverter = function(swapData) {
      let {gameMiniState, piece} = swapData;
      return {gameMiniState, piece: PieceConverter.fromJson(piece)};
    };

    let jsonPiece = PieceConverter.toJson(pc);
    return serviceWrapper(mockErrors, api, api.swap, this.loginEvidence, gameId, jsonPiece)
      .mapValue(swapDataConverter);
  }

  getFullGame(gameId, mockErrors) {
    let api = this.api;
    return serviceWrapper(mockErrors, api, api.getFullGame, this.loginEvidence, gameId)
      .mapValue((data) => GameConverter.fullGameResponseToGame(data));
  }

  close(gameId, mockErrors) {
    let api = this.api;
    return serviceWrapper(mockErrors, api, api.close, this.loginEvidence, gameId);
  }

  suspend(gameId, mockErrors) {
    let api = this.api;
    return serviceWrapper(mockErrors, api, api.suspend, this.loginEvidence, gameId);
  }

  resume(gameId, mockErrors) {
    let api = this.api;
    return serviceWrapper(mockErrors, api, api.resume, this.loginEvidence, gameId);
  }

  cancel(gameId, mockErrors) {
    let api = this.api;
    return serviceWrapper(mockErrors, api, api.cancel, this.loginEvidence, gameId);
  }

  resign(gameId, mockErrors) {
    let api = this.api;
    return serviceWrapper(mockErrors, api, api.resign, this.loginEvidence, gameId);
  }

   getUserGames(fromEpochSecond, toEpochSecond, maxGames, mockErrors) {
    let api = this.api;
    return serviceWrapper(mockErrors, api, api.getUserGames,
      this.loginEvidence, fromEpochSecond, toEpochSecond, maxGames)
      .mapValue(data => data.games); // List of BasicGameInfo.
  }

  getUserGameSettings(mockErrors) {
    let api = this.api;
    return serviceWrapper(mockErrors, api, api.getUserGameSettings, this.loginEvidence)
      .mapValue(data => data ? SettingsConverter.fromJson(data) : null);
  }

  settingsOptionFromJson(json) {
    return (json === null) ? null : SettingsConverter.fromJson(json)
  }

  saveUserGameSettings(settings, mockErrors) {
    let api = this.api;
    let convertedSettings = SettingsConverter.toJson(settings);
    return serviceWrapper(mockErrors, api, api.saveUserGameSettings,
      this.loginEvidence, convertedSettings)
  }
}

export default GameService;
