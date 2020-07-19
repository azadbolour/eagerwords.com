/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */


import {stringify} from "../../base/util/Logger";
import {mkGame, RUN_STATE} from "../domain/Game";
import {mkEmptyBoard} from "../domain/Board";
import {mkTray} from "../domain/Tray";
import {mkPiece} from "../domain/Piece";
import {playPiecesWord} from "../domain/PlayPiece";
import {mkPoint} from "../../plane/domain/Point";
// import {mkPiecePoint} from "../domain/PiecePoint";
import {mkCommittedPlayPiece, mkPlayPiece} from "../domain/PlayPiece";
import {randomPlayerType, randomStartingPlayer} from "../domain/GameSettings";
import {mkMatrix} from "../../plane/domain/Matrix";
import {emptyPlayHistory, mkPlayHistory} from "../domain/PlayHistory";

// TODO. Conversion of class object to json - use JSON.stringify in simple cases.
// TODO. What about conversion from json to a class object?

// Option None becomes json null and sometimes undefined!
export const SettingsConverter = {
  toJson(settings) {
    let startingPlayer = settings.startingPlayer;
    if (startingPlayer === randomPlayerType)
      startingPlayer = null;
    let preferredDevice = settings.preferredDevice;
    if (preferredDevice === undefined)
      preferredDevice = null;
    return {...settings, startingPlayer, preferredDevice}
  },

  fromJson(json) {
    let startingPlayer = json.startingPlayer;
    if (startingPlayer === null || startingPlayer === undefined)
      startingPlayer = randomPlayerType;
    let preferredDevice = json.preferredDevice;
    if (json.preferredDevice === undefined)
      preferredDevice = null;
    return {...json, startingPlayer, preferredDevice}
  }
};

// TODO. URGENT. GameParams should be represented as in the server code: (settings, pointValues).

export const GameParamsConverter = {
  // TODO. Convoluted code. Clean up passage of params from higher level code to API call.
  toJson: function(gameParams) {
    // console.log(`GameParamsConverter - gameParams: ${stringify(gameParams)}`);
    let settings = {...gameParams};
    let pointValues = gameParams.pointValues;
    delete settings.pointValues;
    let rows = pointValues.rows();
    let dto = {settings: SettingsConverter.toJson(settings), pointValues: rows};
    // console.log(`GameParamsConverter - dto: ${stringify(dto)}`);
    return dto;
  },

  fromJson: function(json) {
    let {settings, pointValues} = json;
    let {dimension} = settings;
    let matrixPointValues = mkMatrix(dimension, pointValues);
    let params = {...settings, pointValues: matrixPointValues};
    return params;
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
    let game = this.commonResponseToGame(
      json.gameId, json.boardPiecePoints, json.trayPieces, gameParams, json.userScore, json.machineScore, plays, runState);
    return game;

    // let wordsPlayed = plays.map(play => {
    //   let playerType = play.playerType;
    //   return (play.playType === 'Word') ?
    //       {word: playPiecesWord(play.playPieces), playerType} :
    //       {word: '', playerType}
    //   }
    // );

    // console.log(`full game game: ${stringify(game)}`);
    // console.log(`full game history: ${stringify(history)}`);

    // return {
    //   game,
    //   wordsPlayed: wordsPlayed
    // }

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

  commonResponseToGame: function (gameId, boardPiecePointsJson, trayPiecesJson, gameParams, userScore, machineScore, plays, runState) {
    let trayPieces = trayPiecesJson.map(p => PieceConverter.fromJson(p));
    let tray = mkTray(gameParams.trayCapacity, trayPieces);
    // console.log(`board piece points json: ${stringify(boardPiecePointsJson)}`);
    let playPieces = boardPiecePointsJson.map(gp => {
      let piece = PieceConverter.fromJson(gp.piece);
      let p = gp.point;
      let point = mkPoint(p.row, p.col);
      return mkCommittedPlayPiece(piece, point);
    });
    let dimension = gameParams.dimension;
    let board = mkEmptyBoard(dimension);
    // TODO. May gain some performance by providing board.setPlayPieces.
    // TODO. Or mkBoardFromPlayPieces, avoiding multiple clones.
    playPieces.forEach(playPiece => {
      board = board.setPlayPiece(playPiece);
    });
    let game = mkGame(gameParams, gameId, board, tray,[userScore, machineScore], plays, [], runState);
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
    let game = this.commonResponseToGame(gameId, boardPiecePoints, trayPieces, gameParams, 0, 0, plays, RUN_STATE.RUNNING);
    return game;
  },

  // /**
  //  * @param json - ResumeGameResponse -
  //  *   that is - gameId, gameParams, boardPiecePoints, trayPiece, userScore, machineScore
  //  * @returns {any}
  //  */
  // resumeGameResponseToGame: function(json) {
  //   let {gameId, gameParams, boardPiecePoints, trayPieces, userScore, machineScore} = json;
  //   let uiGameParams = GameParamsConverter.fromJson(gameParams);
  //   let game = this.commonResponseToGame(gameId, boardPiecePoints, trayPieces, uiGameParams, userScore, machineScore);
  //   return game;
  // }
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




