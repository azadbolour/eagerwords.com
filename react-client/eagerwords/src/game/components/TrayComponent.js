/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import PropTypes from 'prop-types';
import PieceComponent from './PieceComponent';
import TraySquareComponent from './TraySquareComponent';
import * as Piece from '../domain/Piece';
import {squareSizeToPieceFont, squareSizeToPixels} from "../domain/GameLookAndFeelParams";

/**
 * A style that includes the board's overall
 * dimensions in pixels, and the layout of its
 * children (the board's squares).
 */
function trayStyle(trayCapacity, squarePixels) {
  const height = trayCapacity * squarePixels;
  const width = squarePixels + 4;
  return {
    display: 'flex',
    flexDirection: 'column',
    border: '2px solid DarkGoldenRod',
    width: width
  };
}

/**
 * A style that includes the dimensions of a board square
 * in pixels.
 */
function squareStyle(squarePixels) {
  let pix = squarePixels + 'px';
  return { width: pix, height: pix };
}

/**
 * User interface component representing tray of available pieces.
 */
const TrayComponent = (props) => {

  /**
   * Return the UI specification of the piece that goes into
   * a specific board square - given the square's position.
   */
  let squareSize = props.squareSize;
  let squarePixels = squareSizeToPixels[squareSize];
  let fontSize = squareSizeToPieceFont[squareSize];
  const renderPiece = function(position) {
    return <PieceComponent
      piece={props.pieces[position]}
      canMovePiece={props.canMovePiece}
      fontSize={fontSize}
    />;
  };

  /**
   * Return the UI specification of a single square based
   * on its position in the tray.
   *
   * A function may return the react specification of a
   * UI components, and these specifications may be composed.
   */
  const renderSquare = function(position) {
    let onRevertMove = props.onRevertMove;
    let squareKey = position;
    let pieces = props.pieces;
    let squareSize = props.squareSize;

    let isTrayPiece = function(piece) {
      let index = pieces.findIndex(pce => Piece.eq(pce, piece));
      let exists = index >= 0;
      return exists;
    };
    let enabled = props.enabled;

    return (
      <div key={squareKey} style={squareStyle({squarePixels})}>
        <TraySquareComponent
          position={position}
          isTrayPiece={isTrayPiece}
          squarePixels={squarePixels}
          enabled={enabled}
          onRevertMove={onRevertMove}
        >
          {renderPiece(position)}
        </TraySquareComponent>
      </div>
    );
  };

  /**
   * Render all the squares on the tray by accumulating their
   * specifications in an array and interpolating the array as
   * the child of a div components. The div components has a style
   * with the correct overall size of the board. See boardStyle.
   */
  let trayCapacity = props.pieces.length;
  let squares = [];

  for (let position = 0; position < trayCapacity; position++)
    squares.push(renderSquare(position));

  return (
    <div style={{align: 'center'}}>
    <div style={trayStyle(trayCapacity, squarePixels)}>
      {squares}
    </div>
    </div>
  );
};

TrayComponent.propTypes = {
  pieces: PropTypes.array.isRequired,
  canMovePiece: PropTypes.func.isRequired,
  squareSize: PropTypes.string.isRequired,
  enabled: PropTypes.bool.isRequired,

  /**
   * Event handler for reverting moves.
   */
  onRevertMove: PropTypes.func.isRequired
};

/**
 * The opponent's tray of pieces.
 *
 * <p>
 * The tray components is decorated with a drag-drop context.
 * The drag-drop contexts mixes in the drag-drop state, allowing
 * dragged elements to be tracked. It also provides the implementation
 * strategy for drag and drop, which, in this case, is the drag-drop
 * mechanism provided by HTML5.
 */
export default TrayComponent;
