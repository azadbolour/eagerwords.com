/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

/**
 * @module GameService
 */

import {gameApis} from '../../game/api/ApiUtil';
import {GameConverter, GameParamsConverter, PieceConverter, UserGameSettingsConverter} from '../api/GameConverters';
import {PlayPieceConverter, PointConverter} from "../api/GameConverters";
import {stringify} from "../../base/util/Logger";
import {getApiType} from "../../envvars";
import {gameResponseToResultPromiseMapper} from "../domain/GameErrors";
import {apiMockErrorAdapter} from "../../base/domain/ApiAdapters";
import {Vow} from "../../base/domain/Vow";

// TODO. URGENT. Refactor the doc comments below based on the new Vow/Result monad.
// Some comments may have to move to the client API.

/**
 *
 * Abstraction layer above the api to hide the api implementation,
 * and to convert from application data structures to api data structures.
 */

/**
 * Mix in mocking of errors for testing, call the API,
 * and package the response in a Vow.
 *
 * In development mode, the UI will provide options for simulating
 * mock errors, and those options will be reflected in the mockErrors
 * parameter.
 *
 * If mock error options are present, the first one will be simulated,
 * and the API call will not be called.
 *
 * In either case, a promise is returned that will be fulfilled
 * by the json returned from the API. In case of an error response from
 * the API, if the error is a user error detected explicitly by the
 * server-side code, the response status of 422 is returned with a json
 * that includes a tag designating the specific error, and parameters
 * describing the error.
 *
 * The Promise[HttpResponse] is transformed to a Vow[T], that represents
 * Promise[Result[T]] where T is the expected type of the happy response.
 *
 * Thus, callers of the service layer always get a Vow[T], which is
 * monad like Scala's Future monad, and can manipulate the response
 * monadically.
 *
 * @param mockErrors Includes an option to mock a type of error.
 * @param api The API object to call.
 * @param func The API function to call.
 * @param args The arguments to the API function.
 *
 * @returns Vow[T] Where T is the type of the expected response.
 *                 In case of an error response, the Vow will
 *                 resolve to an error Result based on the "tag"
 *                 if any returned in the response json.
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
      .mapValue(data => data ? UserGameSettingsConverter.fromJson(data) : null);
  }

  settingsOptionFromJson(json) {
    return (json === null) ? null : UserGameSettingsConverter.fromJson(json)
  }

  saveUserGameSettings(settings, mockErrors) {
    let api = this.api;
    let convertedSettings = UserGameSettingsConverter.toJson(settings);
    return serviceWrapper(mockErrors, api, api.saveUserGameSettings,
      this.loginEvidence, convertedSettings)
  }
}

export default GameService;
