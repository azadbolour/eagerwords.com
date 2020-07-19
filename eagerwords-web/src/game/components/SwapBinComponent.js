/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React from 'react';
import PropTypes from 'prop-types';
import {mkPiece} from '../domain/Piece';
import SquareComponent from './SquareComponent';
// import {stringify} from "../util/Logger";
import * as Style from "../css/Styles";
const ItemTypes = require('./DragDropTypes').ItemTypes;
const DropTarget = require('react-dnd').DropTarget;
const pix=30;
const pixels= pix + 'px';

function colorCodedLegalMoveStyle(colorCoding) {
  return {
    position: 'absolute',
    top: 0,
    left: 0,
    height: 50,
    width: 100,
    zIndex: 2,
    opacity: 0.5,
    backgroundColor: colorCoding
  };
}

/**
 * Style for the square - it is relative to its siblings within its parent.
 */
function squareStyle(color, backgroundColor) {
  return {
    display: 'inlineBlock',
    position: 'relative',
    color: color,
    backgroundColor: backgroundColor,
    width: 100,
    height: '50px',
    textAlign: 'center',
    fontFamily: 'Helvetica',
    fontSize: 15,
    fontWeight: 'bold',
    borderStyle: 'solid',
    paddingTop: '10px',
    borderWidth: '3px',
    borderColor: 'GoldenRod',
    marginBottom: '10px',
  };
}

function innerSquareStyle(pixels, backgroundColor, color) {
  const pix = pixels + 'px';
  const width = (2 * pix) +'px';
  return {
    position: 'relative',
    backgroundColor: backgroundColor,
    color: 'DarkGreen',
    width: 80,
    height: pix,
    lineHeight: pix,
    textAlign: 'center',
    fontFamily: 'Helvetica',
    fontSize: 12,
    padding: '1px',
    zIndex: 1
  };
}

let getMonitorPiece = function(monitor) {
  let pieceItem = monitor.getItem();
  return mkPiece(pieceItem.value, pieceItem.id);
};

const pieceDropper = {
  canDrop: function (props, monitor) {
    let piece = getMonitorPiece(monitor);
    let can = props.isTrayPiece(piece) && props.enabled;
    return can;
  },

  drop: function (props, monitor) {
    let piece = getMonitorPiece(monitor);
    const onSwap = props.onSwap;
    onSwap(piece);
  }
};

function injectedDropTargetProperties(connect, monitor) {
  return {
    connectDropTarget: connect.dropTarget(),
    isOver: monitor.isOver(),
    canDrop: monitor.canDrop()
  };
}

class SwapBinComponent extends React.Component {
  static propTypes = {
    /**
     * Check that piece comes fro the tray - and hence can be swapped.
     */
    isTrayPiece: PropTypes.func.isRequired,

    /**
     * The swap bin is enabled.
     */
    enabled: PropTypes.bool.isRequired,

    /**
     * Is the cursor over the current square?
     */
    isOver: PropTypes.bool.isRequired,

    canDrop: PropTypes.bool.isRequired,

    /**
     * Handler of user actions.
     */
    onSwap: PropTypes.func.isRequired

    // Note connectDropTarget is also injected.
  };

  render() {
    let connectDropTarget = this.props.connectDropTarget;
    let isOver = this.props.isOver;
    let canDrop = this.props.canDrop;
    let labelText = "Swap Drop"; // TODO. Does not wrap inside the square. Fix.
    // let labelText = "Swap";
    let enabled = this.props.enabled;
    // let color = enabled ? 'Chocolate' : Style.disabledColor;
    let color = enabled ? 'White' : 'DarkGrey';
    let backgroundColor = enabled ? 'GoldenRod' : Style.disabledBackgroundColor;
    // TODO. Could not put line break between two words and have the entire text appear in square boundary.

    return connectDropTarget(
      <div style={squareStyle(color, backgroundColor)}>

        {/* <div style={innerSquareStyle(pix, backgroundColor, color)}>
          {labelText}
        </div>
        */}
        {labelText}

        {isOver && !canDrop && <div style={colorCodedLegalMoveStyle('red')} />}
        {!isOver && canDrop && <div style={colorCodedLegalMoveStyle('yellow')} />}
        {isOver && canDrop && <div style={colorCodedLegalMoveStyle('green')} />}

      </div>
    );
  }
}

export default DropTarget(ItemTypes.PIECE, pieceDropper, injectedDropTargetProperties)(SwapBinComponent);
