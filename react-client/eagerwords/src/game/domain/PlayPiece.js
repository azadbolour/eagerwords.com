/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */


import {stringify} from 'lib/js-util/index';
import * as Piece from './Piece';
import {mkPiecePoint} from './PiecePoint';
import {DEAD_PIECE} from "./Piece";

export const MOVED = true;

export const mkPlayPiece = function(piece, point, moved) {
  let _piece = piece;
  let _point = point;
  let _moved = moved;
  let _piecePoint = mkPiecePoint(piece, point);

  return {
    get piecePoint() { return _piecePoint; },
    get moved() { return _moved; },
    get piece() { return _piece; },
    get point() { return _point; },

    putPiece: function(piece) {
      const playPiece = mkPlayPiece(piece, _point, _moved);
      return playPiece;
    },
    hasRealPiece: function() {
      return (!Piece.eq(_piece, Piece.NO_PIECE) && !Piece.eq(_piece, Piece.DEAD_PIECE));
    },
    isFree: function() {
      return Piece.eq(_piece, Piece.NO_PIECE);
    },
    isOriginal: function() {
      return this.hasRealPiece() && !this.moved;
    },
    isDead: function() {
      return Piece.eq(_piece, Piece.DEAD_PIECE);
    },
    setMovedAway: function() {
      if (this.isFree())
        throw {
          name: "illegal state",
          message: `point ${stringify(this.point)} has no piece to have moved away`
        };
      return mkBarePlayPiece(_point);
    },
    setMovedIn: function(piece) {
      return mkPlayPiece(piece, _point, MOVED);
    },
    setCommitted: function() {
      return mkPlayPiece(_piece, _point, !MOVED);
    }
  };
};

export const mkBarePlayPiece = function(point) {
  return mkPlayPiece(Piece.NO_PIECE, point, !MOVED);
};

export const mkCommittedPlayPiece = function(piece, point) {
  return mkPlayPiece(piece, point, !MOVED);
};

export const mkDeadPlayPiece = function(point) {
  return mkPlayPiece(DEAD_PIECE, point, !MOVED);
};

export const mkMovePlayPiece = function(piece, point) {
  return mkPlayPiece(piece, point, MOVED);
};

export const playPiecesWord = function(playPieces) {
  return playPieces.map(it => it.piece.value).join('');
};

export const findFilledSegmentBoundary = function(playPieces, index, direction) {
  let number = playPieces.length;
  let to = index;
  for (let i = index + direction; i >= 0 && i < number && playPieces[i].hasRealPiece(); i = i + direction)
      to = i;
  return to;
};

export const movedPiecePoints = function(playPieces) {
  let movedPlayPieces = playPieces.filter(p => p.moved);
  let moved = movedPlayPieces.map(p => p.piecePoint);
  return moved;
};

