/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import PropTypes from 'prop-types';
import {isDead} from "../domain/Piece";
import {pieceFont} from "../domain/GameSettings";
import {warn, getOrElseF} from '../../base/util/MiscUtil'

import {
  DragSource,
} from 'react-dnd';

const ItemTypes = require('./DragDropTypes').ItemTypes;

const letterStyle = (fontSize) => ({
  fontSize,
  MozUserSelect: 'none',
  WebkitUserSelect: 'none',
  MsUserSelect: 'none',
  userSelect: 'none'
});

const pieceDragger = {
  /**
   * Return the drag item: an arbitrary plain JS object
   * providing information about the dragged element.
   * The drag item is distinct from the source component
   * of the drag.
   */
  beginDrag: function (props) {
    let piece = props.piece;
    // Must return a plain object.
    let item = {
      value: piece.value,
      id: piece.id,
    };
    return item;
  },

  canDrag(props, monitor) {
    let can = props.canMovePiece(props.piece);
    return can;
  },

  endDrag: function(props, monitor, component) {
  }
};

/**
 * The piece. A draggable item.
 */
// class PieceComponent extends React.Component {
const PieceComponent = (props) => {

    let connectDragSource = props.connectDragSource;
    let connectDragPreview = props.connectDragPreview;
    let isDragging = props.isDragging;
    let letter = props.piece.value;
    let fontSize = props.fontSize;

    // let fontSize = getOrElseF(pieceFont, props.size, () => {
    //   warn("PieceComponent: can't get font size for size:", props.size)
    //   return 12;
    // });
    if (isDead(letter))
      letter = ""; // Do not render "dead" piece.
    // let worth = Piece.worths[letter];

    // let theDiv =
    return (
      <>
      <div
        ref={isDragging ? connectDragPreview : connectDragSource}
        style={{
          opacity: isDragging ? 0.5 : 1,
          fontSize: 15,
          fontWeight: 'bold',
          cursor: 'move',
          zIndex: 10
        }}
      >
        <div style={letterStyle(fontSize)}>
          {letter}
        </div>
      </div>
      </>
    );
};

PieceComponent.propTypes = {

  /**
   * The immutable state of the piece.
   * TODO. How to add isRequired with type constraint?
   */
  piece: PropTypes.object.isRequired,

  /**
   * Function that takes a piece and determines if it can be moved
   * (based on its current location and move status).
   */
  canMovePiece: PropTypes.func.isRequired,

  /**
   * Injected - obtained from the 'collect' function.
   */
  connectDragSource: PropTypes.func.isRequired,

  connectDragPreview: PropTypes.func.isRequired,

  /**
   * Injected - obtained from the 'collect' function.
   */
  isDragging: PropTypes.bool.isRequired,

  fontSize: PropTypes.number.isRequired
};

export default DragSource(
  ItemTypes.PIECE, pieceDragger,
  (connect, monitor) => ({
    connectDragSource: connect.dragSource(),
    connectDragPreview: connect.dragPreview(),
    isDragging: monitor.isDragging(),
  })
)(PieceComponent)
