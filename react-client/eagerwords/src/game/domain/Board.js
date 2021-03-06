/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */


import {valueResult} from "../../lib/js-util/domain/Result";
import {mkBarePlayPiece, mkCommittedPlayPiece, findFilledSegmentBoundary, mkDeadPlayPiece} from './PlayPiece';
import * as Piece from './Piece';
import {mkPoint} from '../../grid/domain/Point';
import {mkMatrixFromCoordinates} from '../../grid/domain/Matrix';
import {stringify} from 'lib/js-util/index';
import {errorTags, mkUiErrorResult} from "./GameErrors";
import {MiscUtil} from 'lib/js-util/index';

let {range} = MiscUtil;

export const mkEmptyBoard = function(dimension) {
  let matrix = mkMatrixFromCoordinates(dimension, function(row, col) {
    return mkBarePlayPiece(mkPoint(row, col));
  });
  return mkBoard(matrix);
};

export const mkBoard = function(matrix) {

  let _matrix = matrix;
  let _dimension = matrix.dimension;

  return {

    get dimension() { return _dimension; },
    get matrix() { return _matrix; },

    rows: () => matrix.rows(),
    cols: () => matrix.cols(),
    isEmpty: () => _matrix.every(function (playPiece) {return playPiece.isFree()}),
    isFull: () => _matrix.every(function (playPiece) {return !playPiece.isFree()}),

    getPlayPiece: (point) => _matrix.getElement(point),
    findPiece: (piece) => _matrix.find(playPiece => Piece.eq(piece, playPiece.piece)),

    numMoves: function () {
      return _matrix.reduce(function (count, playPiece) {
        let num = count;
        if (playPiece.moved)
          num += 1;
        return num;
      }, 0);
    },

    playPieces: function() {
      return _matrix.linearize().filter(function(playPiece) {
        return playPiece.hasRealPiece();
      })
    },

    hasCommittedPlays: () => _matrix.some(playPiece => playPiece.isOriginal()),

    setPlayPiece: function(playPiece) {
      let $matrix = _matrix.setElement(playPiece.point, playPiece);
      return mkBoard($matrix);
    },

    getUserMovePlayPieces: function() {
      return _matrix.reduce(function(playPieces, playPiece) {
        if (playPiece.moved)
          playPieces.push(playPiece);
        return playPieces;
      }, []);
    },

    isMovedPiece(piece) {
      let movePlayPieces = this.getUserMovePlayPieces();
      let isMoved = movePlayPieces.some(it => Piece.eq(it.piece, piece));
      return isMoved;
    },

    commitUserMoves: function() {
      let playPieces = this.getUserMovePlayPieces();
      // TODO. Clone the board just once and change all play pieces. Optimization.
      let $board = this;
      playPieces.forEach(playPiece => {
        $board = $board.setPlayPiece(playPiece.setCommitted());
      });
      return $board;
    },

    rollbackUserMoves: function() {
      let $board = this;
      let playPieces = this.getUserMovePlayPieces();
      playPieces.forEach(playPiece => {
        $board = $board.setPlayPiece(mkBarePlayPiece(playPiece.point));
      });
      return $board;
    },

    setDeadPoints: function(points) {
      let $board = this;
      points.forEach(point => {
        $board = $board.setPlayPiece(mkDeadPlayPiece(point));
      });
      return $board;
    },

    commitMachineMoves: function(movePiecePoints) {
      let $board = this;
      movePiecePoints.forEach(move => {
        $board = $board.setPlayPiece(mkCommittedPlayPiece(move.piece, move.point));
      });
      return $board;
    },

    isFree(point) {
      return this.getPlayPiece(point).isFree();
    },

    isDead(point) {
      return this.getPlayPiece(point).isDead();
    },

    isMoved(point) {
      return this.getPlayPiece(point).moved;
    },

    isOriginal(point) {
      return this.getPlayPiece(point).isOriginal();
    },

    /**
     * Is a move legal for the current state of the board?
     *
     * @param point Trying to move to this position.
     */
    legalMove(point) {
      return this.isFree(point);
    },

    /**
     * Get the play pieces for the supposedly completed play.
     * If the play is incomplete or illegal, throw an appropriate error.
     */
    completedPlayPieces() {
      let playRowsData = this.playLinesData("X");
      let playColsData = this.playLinesData("Y");

      let numPlayRows = playRowsData.length;
      let numPlayCols = playColsData.length;

      if (numPlayRows === 0 || numPlayCols === 0)
        return mkUiErrorResult(errorTags.noMoves);

      if (numPlayRows > 1 && numPlayCols > 1)
        return mkUiErrorResult(errorTags.multiplePlayLines);

      // console.log(`${numPlayRows} ${numPlayCols}`);
      let playLineData = undefined;
      let playStripResult = undefined;
      let playStrip = undefined;

      if (numPlayRows === 1 && numPlayCols === 1) {
        let playLineDataAndPlayStripResult =
          this.determineSingleMovePlayLine(playRowsData[0], playColsData[0]);
        if (playLineDataAndPlayStripResult.isError)
          return playLineDataAndPlayStripResult;
        ({playLineData, playStrip} = playLineDataAndPlayStripResult.data);
      }
      else {
        // Play line is the unique line in a given direction that contains all the moves.
        playLineData = numPlayRows === 1 ? playRowsData[0] : playColsData[0];
        playStripResult = this.getPlayStrip(playLineData);
        if (playStripResult.isError)
          return playStripResult;
        playStrip = playStripResult.data;
      }

      // console.log(`playLineData: ${stringify(playLineData)}`);
      // console.log(`playStrip: ${stringify(playStrip)}`);

      let {numMoves} = this.lineMoveInfo(playLineData);

      // The very first play is unrestricted.
      if (!this.hasCommittedPlays())
        return valueResult(playStrip);

      let hasAnchor = playStrip.length - numMoves > 0;
      if (hasAnchor)
        return valueResult(playStrip);

      return mkUiErrorResult(errorTags.disconnectedWord);
    },

    /**
     * Get lines in a given direction that contain moves: the play lines.
     *
     * @param axis The direction of the lines.
     */
    playLinesData(axis) {
      let lines = axis === "X" ? this.rows() : this.cols();
      let lineNumbers = range(_dimension);
      let linesData = lineNumbers.map(lineNumber => {
        let line = lines[lineNumber];
        let hasMoves = line.some(playPiece => playPiece.moved);
        return { axis, lineNumber, line, hasMoves};
      });
      return linesData.filter(_ => _.hasMoves);
    },

    /**
     * Get information about the moves in a given line.
     * @param playLineData Includes the line number and the line.
     */
    lineMoveInfo(playLineData) {
      let {lineNumber, line} = playLineData;
      let firstMoveIndex = undefined;
      let lastMoveIndex = undefined;
      let hasCenterMove = false;
      let numMoves = 0;
      // TODO. Center is no longer relevant. Remove center processing.
      let center = Math.floor(_dimension / 2);

      for (let i = 0; i < _dimension; i++) {
        if (!line[i].moved)
          continue;
        if (lineNumber === center && i === center)
          hasCenterMove = true;
        if (firstMoveIndex === undefined)
          firstMoveIndex = i;
        lastMoveIndex = i;
        numMoves++;
      }

      // TODO. URGENT. Add an assertion error to the errorTags - unrecoverable.
      // TODO. Convert to use the assertion error tag. Check all callers.
      if (numMoves === 0)
        throw {
          name: "illegal state",
          message: `line ${lineNumber} was expected to have moves but contains none`
        };

      let interMoveFreeSlots = 0; // Empty or dead slots in-between moves.
      let interMoveDeadSlots = 0; // Dead slots in-between moves.

      for (let i = firstMoveIndex + 1; i <= lastMoveIndex - 1; i++) {
        if (this.isFree(line[i].point))
          interMoveFreeSlots += 1;
        if (this.isDead(line[i].point))
          interMoveDeadSlots += 1;
      }

      let isContiguous = interMoveFreeSlots === 0 && interMoveDeadSlots === 0;

      return {
        numMoves, firstMoveIndex, lastMoveIndex, isContiguous, hasCenterMove
      };
    },

    /**
     * Get the strip of a line that contains the entire word play,
     * including moves and existing tiles. Returns the ordered
     * list of tiles (as play pieces).
     *
     * @param playLineData Provides the line.
     *
     */
    getPlayStrip(playLineData) {
      let {line} = playLineData;
      let {firstMoveIndex, lastMoveIndex, isContiguous} =
        this.lineMoveInfo(playLineData);

      if (!isContiguous)
        return mkUiErrorResult(errorTags.incompleteWord);
        // throw errorTags.incompleteWord;

      let beginIndex = this.extendsTo(line, firstMoveIndex, -1);
      let endIndex = this.extendsTo(line, lastMoveIndex, +1);

      let playStrip = line.slice(beginIndex, endIndex + 1); // Slice is right-exclusive.
      return valueResult(playStrip);
    },

    /**
     * In case there is just one move in the play being committed,
     * determine whether the principle axis of the play is horizontal
     * or vertical and get the strip of the play. The principle axis
     * is one in which the move is connected to an existing tile.
     *
     * @param rowLineData Data about the unique play row.
     * @param colLineData Data about the unique play column.
     */
    determineSingleMovePlayLine(rowLineData, colLineData) {
      // console.log(`rowLineData: ${stringify(rowLineData)}`);
      // console.log(`colLineData: ${stringify(colLineData)}`);
      let rowPlayStripResult = this.getPlayStrip(rowLineData);
      if (rowPlayStripResult.isError)
        return rowPlayStripResult;
      let colPlayStripResult = this.getPlayStrip(colLineData);
      if (colPlayStripResult.isError)
        return colPlayStripResult;
      let rowPlayStrip = rowPlayStripResult.data;
      let colPlayStrip = colPlayStripResult.data;

      // The very first play is allowed to include just one letter (e.g., "a").
      // Otherwise a single letter play must be disconnected.
      if (rowPlayStrip.length === 1 && colPlayStrip.length === 1 && this.hasCommittedPlays())
        return mkUiErrorResult(errorTags.disconnectedWord);

      let playLineData = rowPlayStrip.length > 1 ? rowLineData : colLineData;
      let playStrip = rowPlayStrip.length > 1 ? rowPlayStrip : colPlayStrip;

      return valueResult({ playLineData, playStrip });
    },

    // TODO. Remove. Replace with the inner call.
    extendsTo(playPieces, index, direction) {
      return findFilledSegmentBoundary(playPieces, index, direction);
    }
  };
};
