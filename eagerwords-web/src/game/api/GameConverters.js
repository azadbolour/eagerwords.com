/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */


import {stringify} from "../../base/util/Logger";
import {mkGame, RUN_STATE} from "../domain/Game";
import {mkEmptyBoard} from "../domain/Board";
import {mkTray} from "../domain/Tray";
import {mkPiece} from "../domain/Piece";
import {playPiecesWord} from "../domain/PlayPiece";
import {mkPoint} from "../../plane/domain/Point";
import {mkCommittedPlayPiece, mkPlayPiece} from "../domain/PlayPiece";
import {randomPlayerType} from "../domain/GamePlayParams";
import {mkMatrix} from "../../plane/domain/Matrix";
import {emptyPlayHistory, mkPlayHistory} from "../domain/PlayHistory";
import {mkGameParams} from "../domain/GameParams";

// TODO. Conversion of class object to json - use JSON.stringify in simple cases.
// TODO. What about conversion from json to a class object?

// Random starting player represented as null on the wire, Option.None server.
export const GamePlaySettingsConverter = {
  toJson(playSettings) {
    let startingPlayer = playSettings.startingPlayer;
    if (startingPlayer === randomPlayerType)
      startingPlayer = null;
    return {...playSettings, startingPlayer};
  },
  fromJson(json) {
    let startingPlayer = json.startingPlayer;
    if (startingPlayer === null || startingPlayer === undefined)
      startingPlayer = randomPlayerType;
    return {...json, startingPlayer};
  }
};

// Random device type represented as null on the wire, Option.None in server.
export const GameLookAndFeelSettingsConverter = {
  toJson(lookAndFeelSettings) {
    let preferredDevice = lookAndFeelSettings.preferredDevice;
    if (preferredDevice === undefined) // For good measure. Should be null for no preference.
      preferredDevice = null;
    return {...lookAndFeelSettings, preferredDevice};
  },
  fromJson(json) {
    let preferredDevice = json.preferredDevice;
    if (json.preferredDevice === undefined) // For good measure. Should be null for None.
      preferredDevice = null;
    return {...json, preferredDevice};
  }
};

export const UserGameSettingsConverter = {
  toJson(userGameSettings) {
    let {playSettings, lookAndFeelSettings} = userGameSettings;
    return {
      playSettings: GamePlaySettingsConverter.toJson(playSettings),
      lookAndFeelSettings: GameLookAndFeelSettingsConverter.toJson(lookAndFeelSettings)
    }
  },
  fromJson(json) {
    let {playSettings, lookAndFeelSettings} = json;
    return {
      playSettings: GamePlaySettingsConverter.fromJson(playSettings),
      lookAndFeelSettings: GameLookAndFeelSettingsConverter.fromJson(lookAndFeelSettings)
    }
  }
};

// TODO. URGENT. GameParams should be represented as in the server code: (settings, pointValues).

export const GameParamsConverter = {
  // TODO. Convoluted code. Clean up passage of params from higher level code to API call.
  toJson: function(gameParams) {
    let playParams = gameParams.playParams;
    playParams = GamePlaySettingsConverter.toJson(playParams);
    let matrixPointValues = gameParams.pointValues;
    let pointValues = matrixPointValues.rows();
    let jsonParams = {playParams, pointValues};
    return jsonParams;
  },

  fromJson: function(json) {
    let {playParams, pointValues} = json;
    playParams = GamePlaySettingsConverter.fromJson(playParams);
    let {dimension} = playParams;
    let matrixPointValues = mkMatrix(dimension, pointValues);
    let gameParams = mkGameParams(playParams, matrixPointValues);
    return gameParams;
  }
};

export const GameConverter = {

  // TODO. Constants.
  serverStateToRunState: function(serverState) {
    switch(serverState) {
      case 'RESIGNED':
      case 'ENDED':
        return RUN_STATE.FINISHED;
      default:
        return RUN_STATE.RUNNING;
    }
  },

  fullGameResponseToGame: function(json) {
    let params = json.gameParams;
    let gameParams = GameParamsConverter.fromJson(params);
    let plays = this.convertPlays(json.plays);
    let serverState = json.state;
    let runState = this.serverStateToRunState(serverState);
    let deadPointsJson = json.deadPoints;
    let game = this.commonResponseToGame(
      json.gameId, json.boardPiecePoints, deadPointsJson, json.trayPieces, gameParams, json.userScore, json.machineScore, plays, runState);
    return game;
  },

  convertPlays: function(plays) {
    let wordsPlayed = plays.map(play => {
        let playerType = play.playerType;
        return (play.playType === 'Word') ?
          {word: playPiecesWord(play.playPieces), playerType} :
          {word: '', playerType}
      }
    );
    return mkPlayHistory(wordsPlayed);
  },

  commonResponseToGame: function (gameId, boardPiecePointsJson, deadPointsJson, trayPiecesJson, gameParams, userScore, machineScore, plays, runState) {
    let trayPieces = trayPiecesJson.map(p => PieceConverter.fromJson(p));
    let tray = mkTray(gameParams.playParams.trayCapacity, trayPieces);
    // console.log(`board piece points json: ${stringify(boardPiecePointsJson)}`);
    let playPieces = boardPiecePointsJson.map(gp => {
      let piece = PieceConverter.fromJson(gp.piece);
      let p = gp.point;
      let point = mkPoint(p.row, p.col);
      return mkCommittedPlayPiece(piece, point);
    });
    let dimension = gameParams.playParams.dimension;
    let board = mkEmptyBoard(dimension);
    // TODO. May gain some performance by providing board.setPlayPieces.
    // TODO. Or mkBoardFromPlayPieces, avoiding multiple clones.
    playPieces.forEach(playPiece => {
      board = board.setPlayPiece(playPiece);
    });
    let deadPoints = deadPointsJson.map(p => mkPoint(p.row, p.col));
    let $board = board.setDeadPoints(deadPoints);
    let game = mkGame(gameParams, gameId, $board, tray,[userScore, machineScore], plays, [], runState);
    return game;
  },

  /**
   * @param gameDto = StartGameResponse
   *   that is - (gameId, boardPiecePoints, trayPieces).
   * @param gameParams Comes back in the json - but we already know it - avoid extra work.
   */
  startGameResponseToGame: function(gameDto, gameParams) {
    let {gameId, boardPiecePoints, trayPieces} = gameDto;
    let plays = emptyPlayHistory();
    let game = this.commonResponseToGame(gameId, boardPiecePoints, [], trayPieces, gameParams, 0, 0, plays, RUN_STATE.RUNNING);
    return game;
  },
};

export const PieceConverter = {
  fromJson: function(json) {
    return mkPiece(json.value, json.id);
  },

  toJson: function(piece) {
    return {
      value: piece.value,
      id: piece.id
    }
  }
};

export const PlayPieceConverter = {
  toJson: function(playPiece) {
    let point = playPiece.point;
    return {
      piece: PieceConverter.toJson(playPiece.piece),
      point: {row: point.row, col: point.col},
      moved: playPiece.moved
    };
  },

  fromJson: function(json) {
    let piece = PieceConverter.fromJson(json.piece);
    let point = mkPoint(json.point.row, json.point.col);
    return mkPlayPiece(piece, point, json.moved);
  }
};

export const PointConverter = {
  fromJson: function(json) {
    return mkPoint(json.row, json.col);
  }
};

export const GameSummaryConverter = {
  fromJson: function(json) {
    return json;
  }
};
