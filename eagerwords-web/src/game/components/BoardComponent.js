/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import PropTypes from 'prop-types';
import BoardSquareComponent from './BoardSquareComponent';
import PieceComponent from './PieceComponent';
import {mkPoint} from '../../plane/domain/Point';
import * as Point from '../../plane/domain/Point';
import {repeatF} from '../../base/util/MiscUtil';
import {squareSizeToPieceFont, squareSizeToPixels} from "../domain/GameLookAndFeelParams";

// import {stringify} from "../util/Logger";

/**
 * A style that includes the board's overall
 * dimensions in pixels, and the layout of its
 * children (the board's squares).
 */
function boardStyle(dimension, squarePixels) {
  let pixels = dimension * squarePixels;
  return {
    width: pixels + 'px',
    height: pixels + 'px',
    display: 'flex',
    flexWrap: 'wrap',
  };
}

/**
 * A style that includes the dimensions of a board square
 * in pixels.
 */
function squareStyle(squarePixels) {
  let pix = squarePixels + 'px';
  return { width: pix, height: pix, position: 'relative' }
}

/**
 * User interface component representing a board.
 */
const BoardComponent = (props) => {

  /**
   * Return the UI specification of the piece that goes into
   * a specific board square - given the square's position.
   */
  const renderPiece = function (point) {
    let piece = props.board.rows()[point.row][point.col].piece;
    let canMovePiece = props.canMovePiece;
    let enabled = props.enabled;
    // let size = safeSquarePixelsToSize(props.squarePixels);
    // piece = (piece) ? piece : Piece.NO_PIECE;
    let fontSize = squareSizeToPieceFont[props.squareSize];
    return <PieceComponent
      piece={piece}
      fontSize={fontSize}
      canMovePiece={canMovePiece}
      enabled={enabled}
    />;
  };

  /**
   * Return the UI specification of a single square based
   * on it row, col coordinates.
   *
   * A function may return the react specification of a
   * UI components, and these specifications may be composed.
   */
  const renderSquare = function (row, col) {
    let gameHandler = props.gameHandler;
    let dimension = props.board.dimension;
    let squareKey = dimension * row + col;
    let isLegalMove = props.isLegalMove;
    // let squarePixels = props.squarePixels;
    // let squareSize = safeSquarePixelsToSize(squarePixels);
    // let squareSize = props.squareSize;
    let squareSize = props.squareSize;
    let squarePixels = squareSizeToPixels[squareSize];
    let point = mkPoint(row, col);
    let inPlay = props.pointsInUserPlay.some(p => Point.eq(p, point));
    let justFilledByMachine = props.pointsMovedInMachinePlay.some(p => Point.eq(p, point));
    let enabled = props.enabled;
    let pointValue = props.pointValues.getElement(point);
    let squarePiece = props.board.rows()[row][col].piece;
    let onMove = props.onMove;

    return (
      <div key={squareKey} style={squareStyle({squarePixels})}>
        <BoardSquareComponent
          inPlay={inPlay}
          justFilledByMachine={justFilledByMachine}
          point={point}
          piece={squarePiece}
          isLegalMove={isLegalMove}
          squarePixels={squarePixels}
          squareSize={squareSize}
          pointValue={pointValue}
          enabled={enabled}
          onMove={onMove}
        >
          {renderPiece(point)}
        </BoardSquareComponent>
      </div>
    );
  };

  /**
   * Render all the squares on the board by accumulating their
   * components objects in an array and interpolating the array as
   * the child of a div components. The div components has a style
   * with the correct overall size of the board.
   */
  let dimension = props.board.dimension;
  // let squarePixels = props.squarePixels;
  // let squares = [];

  // for (let r = 0; r < dimension; r++) {
  //   let row = [];
  //   for (let c = 0; c < dimension; c++)
  //     row.push(renderSquare(r, c));
  //   squares.push(row);
  // }

  const renderRow = function(r) {
    let row = repeatF(dimension, (i) => renderSquare(r, i));
    let rowKey = r;
    return <div key={rowKey} style={{display: 'flex', flexDirection: 'row'}}>{row}</div>
  };

  const renderBoard = function() {
    let rows = repeatF(dimension, (i) => renderRow(i));
    return <div>
      <div style={{display: 'flex', flexDirection: 'column', border: '2px solid DarkGoldenRod'}}>{rows}</div>
    </div>
  };

  return renderBoard()
};

BoardComponent.propTypes = {
  /**
   * The board data.
   */
  board: PropTypes.object.isRequired,

  /**
   * Positions that are currently in play by the user - i.e. occupied by pieces.
   */
  pointsInUserPlay: PropTypes.array.isRequired,

  /**
   * Points that were just filled by the machine.
   */
  pointsMovedInMachinePlay: PropTypes.array.isRequired,

  /**
   * Function of position that determines whether the position
   * is a legal destination of a move - whether a piece is allowed
   * to be moved to that position given the current state of the game.
   */
  isLegalMove: PropTypes.func.isRequired,

  canMovePiece: PropTypes.func.isRequired,

  squareSize: PropTypes.string.isRequired,

  // squareSize: PropTypes.string.isRequired,

  pointValues: PropTypes.object.isRequired,

  /**
   * The board responds to interactions.
   */
  enabled: PropTypes.bool.isRequired,

  /**
   * Handler of move onto the board.
   */
  onMove: PropTypes.func.isRequired
};

export default BoardComponent;
