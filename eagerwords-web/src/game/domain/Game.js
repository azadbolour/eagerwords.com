/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */


import {stringify} from "../../base/util/Logger";
import * as Board from './Board';
// import * as Point from './Point';
import * as Tray from './Tray';
import * as PlayPiece from './PlayPiece';
import {mkMovePlayPiece} from './PlayPiece';
// import {mkMultiplierGrid} from "./ScoreMultiplier";
import * as PointValue from './PointValue';
import {emptyPlayHistory} from "./PlayHistory";
import {PlayerType} from "../api/PlayerType";
import {playPiecesWord} from "./PlayPiece";

export const USER_INDEX = 0;
export const MACHINE_INDEX = 1;

const EMPTY_GAME_ID = "emptyGame";

export const RUN_STATE = {
  PRE_START: "pre-start",
  RUNNING: "running",
  FINISHED: "finished",
  KILLED: "killed"
};

export const noGame = game => game === undefined || game.terminated();

export const logPostMortemGame = () => console.warn('warning: attempt to operate on a terminated game');

export const mkEmptyGame = function(settings) {
  const board = Board.mkEmptyBoard(settings.dimension);
  const tray = Tray.mkEmptyTray(settings.trayCapacity);
  const valueFactory = PointValue.mkValueFactory(settings.dimension);
  const pointValues = valueFactory.mkEmptyValueGrid();
  const gameParams = {...settings, pointValues};
  const score = [0, 0];
  const plays = emptyPlayHistory();
  console.log(`empty plays: ${stringify(plays)}`);
  return mkGame(gameParams, EMPTY_GAME_ID, board, tray, score, plays, [], RUN_STATE.PRE_START);
};

export const mkGame = function(gameParams, gameId, board, tray, score, plays, machineMoves = [], runState = RUN_STATE.RUNNING, stopInfo = null) {
  let _gameParams = gameParams;
  let _gameId = gameId;
  let _board = board;
  let _tray = tray;
  let _dimension = gameParams.dimension;
  let _squarePixels = gameParams.squarePixels;
  let _trayCapacity = gameParams.trayCapacity;
  let _pointValues = gameParams.pointValues;
  let _score = score;
  let _plays = plays;
  let _machineMoves = machineMoves;
  let _runState = runState;
  let _stopInfo = stopInfo;

  let updateScore = function(playerIndex, playScore) {
    let updatedScore = _score.slice();
    updatedScore[playerIndex] += playScore;
    return updatedScore;
  };

  // Need a revert here - so that applyBoardMove can be implemented as a revert + a tray move.

  return {
    get gameParams() { return _gameParams; },
    get gameId() { return _gameId; },
    get board() { return _board; },
    get tray() { return _tray; },
    get dimension() { return _dimension; },
    get squarePixels() { return _squarePixels; },
    get trayCapacity() { return _trayCapacity; },
    get pointValues() { return _pointValues; },
    get score() { return _score.slice(); },
    get plays() { return _plays; },
    get machineMoves() { return _machineMoves.slice(); },
    get runState() {return _runState; },
    get stopInfo() {return _stopInfo; },

    notStarted: () => _runState === RUN_STATE.PRE_START,
    isFilled: () => _board.isFull(),
    running: () => _runState === RUN_STATE.RUNNING,
    finished: () => _runState === RUN_STATE.FINISHED,

    terminated: function() {
      return !this.running();
    },

    kill: function() {
      let $game = mkGame(_gameParams, _gameId, _board, _tray, _score, _plays,[], RUN_STATE.KILLED);
      return $game;
    },

    // TODO. Move canMove function to TrayComponent and BoardComponent.
    // It is a user interaction issue and belongs to components.
    canMovePiece: function(piece) {
      if (_tray.findIndexByPieceId(piece.id) >= 0)
        return true;
      return _board.isMovedPiece(piece);
    },

    // TODO. Better to return flag indicating move was successful, as well as mutated game.

    applyUserMove: function(move) {
      const { piece, point } = move;
      if (!this.legalMove(piece, point)) {
        console.warn(`attempt to apply illegal move ${stringify(move)} - ignored`);
        return this;
      }

      const trayIndex = _tray.findIndexByPieceId(piece.id);
      const sourcePlayPiece = _board.findPiece(piece);

      const isFromTray = trayIndex >= 0;
      const isFromBoard = sourcePlayPiece !== undefined;

      if (isFromTray && isFromBoard)
        throw {
          name: "illegal state",
          message: `piece ${stringify(piece)} belongs to both the tray and the board`
        };

      if (isFromTray)
        return this.applyTrayMove(move);
      else
        return this.applyBoardMove(move);
    },

    applyBoardMove: function(move) {
      const { piece } = move;
      // let sourcePoint = _board.findPiece(piece);
      let $game = this.revertMove(piece);
      return $game.applyTrayMove(move);
    },

    applyTrayMove: function(move) {
      const { piece, point } = move;
      let playPiece = mkMovePlayPiece(piece, point);
      let $tray = _tray.removePiece(piece.id);
      let $board = _board.setPlayPiece(playPiece);
      let $game = mkGame(_gameParams, _gameId, $board, $tray, _score, _plays);
      return $game;
    },

    getUserMovePlayPieces: () => _board.getUserMovePlayPieces(),

    wordPlayStarted: function() {
      return (this.getUserMovePlayPieces().length > 0)
    },

    /**
     * Throws exception if play is illegal.
     */
    getCompletedPlayPieces: () => _board.completedPlayPieces(), // Throws.

    commitUserMoves: function(committedPlayPieces, playScore, replacementPieces, deadPoints) {
      let word = playPiecesWord(committedPlayPieces);
      let $plays = _plays.pushWordPlayed(word, PlayerType.user);

      let $tray = _tray.addPieces(replacementPieces);
      let $score = updateScore(USER_INDEX, playScore);
      let $board = _board.commitUserMoves();
      let $game = mkGame(_gameParams, _gameId, $board, $tray, $score, $plays);
      $game = $game.setDeadPoints(deadPoints);
      return $game;
    },

    commitMachineMoves: function(playScore, playedPieces, deadPoints) {
      let movedPiecePoints = PlayPiece.movedPiecePoints(playedPieces);
      let $plays = _plays.pushWordPlayed(playPiecesWord(playedPieces), PlayerType.machine);
      let $board = _board.commitMachineMoves(movedPiecePoints);
      let $score = updateScore(MACHINE_INDEX, playScore);
      let $game = mkGame(_gameParams, _gameId, $board, _tray, $score, $plays, movedPiecePoints);
      $game = $game.setDeadPoints(deadPoints);
      return $game;
    },

    setDeadPoints: function(points) {
      let $board = _board.setDeadPoints(points);
      return mkGame(_gameParams, _gameId, $board, _tray, _score, _plays, _machineMoves);
    },

    end: function(stopInfo) {
      let $game = mkGame(_gameParams, _gameId, _board, _tray, _score, _plays, [], RUN_STATE.FINISHED, stopInfo);
      return $game;
    },

    revertPlay: function() {
      let movedPlayPieces = this.getUserMovePlayPieces();
      let movedTrayPieces = movedPlayPieces.map(playPiece => playPiece.piece)
      let $board = _board.rollbackUserMoves();
      let $tray = _tray.addPieces(movedTrayPieces);
      let $game = mkGame(_gameParams, _gameId, $board, $tray, _score, _plays);
      return $game;
    },

    numPiecesInPlay: function() {
      return this.getUserMovePlayPieces().length;
    },

    revertMove: function(piece) {
      let sourcePlayPiece = _board.findPiece(piece);
      let point = sourcePlayPiece.point;

      if (point === undefined) {
        console.error(`attempt to revert move of piece: ${stringify(piece)} which does not belong to the board - ignored`);
        return;
      }
      let barePlayPiece = PlayPiece.mkBarePlayPiece(point);
      let $board = _board.setPlayPiece(barePlayPiece);
      let $tray = _tray.addPiece(piece);
      let $game = mkGame(_gameParams, _gameId, $board, $tray, _score, _plays);
      return $game;
    },

    legalMove: function(piece, point) {
      let onTray = _tray.findIndexByPieceId(piece.id) >= 0;
      let sourcePlayPiece = _board.findPiece(piece);
      let onBoard = sourcePlayPiece !== undefined;

      if (onTray && onBoard) {
        console.error(`move error: piece: ${stringify(piece)}, point: ${stringify(point)}, source play piece: ${stringify(sourcePlayPiece)}`)
        this.logGameState();
        console.trace();
        throw {
          name: "illegal state",
          message: `piece ${stringify(piece)} on tray and on board at the same time`
        };
      }

      // Cannot move a committed piece.
      if (onBoard && sourcePlayPiece.isOriginal())
        return false;

      // Intra-board move is equivalent to a revert followed by a tray move.

      let testGame = this;
      if (onBoard)
        testGame = this.revertMove(piece);
      let testBoard = testGame.board;
      let legal = testBoard.legalMove(point);
      return legal;
    },

    swapTrayPiece: function(replacedPieceId, replacementPiece) {
      let $tray = _tray.replacePiece(replacedPieceId, replacementPiece);
      const updatedPlays = _plays.pushWordPlayed("", PlayerType.user);
      return mkGame(_gameParams, _gameId, _board, $tray, _score, updatedPlays);
    },

    logGameState: function() {
      let playPieces = _board.playPieces();
      console.log("-- The Board --");
      playPieces.forEach(function(pp) {
        console.log(`piece: ${stringify(pp.piece)}, point: ${stringify(pp.point)}. moved: ${pp.moved}`);
      });
      console.log("-- The Tray --");
      _tray.pieces.forEach(function(p) {
        console.log(`${stringify(p)}`);
      });
    }
  };
};
