/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import ButtonBase from "react-bootstrap/Button";
import Dropdown from "react-bootstrap/Dropdown";
import React from "react";

export const playLabelStyle = {
  color: 'DarkGoldenRod',
  align: 'left',
  fontSize: 13,
  fontFamily: 'Arial',
  fontWeight: 'bold',
  margin: 'auto',
  right: '2px'
};

export const playStatusStyle = {
  color: 'Red',
  backgroundColor: 'LightYellow',
  align: 'left',
  fontSize: 16,
  fontWeight: 'bold',
  letterSpacing: '1px',
  margin: '5px',
  minWidth: '500px',
  maxWidth: '500px'
};

export const playScoreStyle = {
  color: 'DarkGoldenRod',
  align: 'left',
  fontSize: 13,
  fontWeight: 'bold',
  letterSpacing: '1px',
  margin: 'auto',
  padding: '1px'
};

export const playLightMessageStyle = function(visible) {
  let display = visible ? 'inline' : 'none';
  return {
    color: 'Maroon',
    align: 'left',
    fontFamily: 'Arial',
    fontSize: 12,
    letterSpacing: '0.5px',
    display: display
  }
};

export const playWordListStyle = {
  overflow: 'auto',
  minHeight: 25,
  width: 100,
  maxHeight: 200,
  borderStyle: 'solid',
  borderWidth: '3px',
  borderColor: 'DarkGrey',
  backgroundColor: 'Khaki',
  marginTop: '10px',
  padding: '4px'
};

export const playScoreBoxStyle = {
  border: '2px solid DarkGoldenRod',
  display: 'inline-block',
  paddingLeft: '5px',
  backgroundColor: 'CornSilk',
  width: '100px'
};

export function PlayButton(props) {
  let disabled = props.disabled;
  // override bootstrap's 'pointer' cursor if the button is disabled.
  let style = disabled ? { cursor: 'inherit' } : {};
  return <ButtonBase {...props} style={{...props.style, ...style}} variant="success" size="sm" />
}

export function PlayDropdownToggle(props) {
  let disabled = props.disabled;
  // override bootstrap's 'pointer' cursor if the dropdown is disabled.
  let style = disabled ? { cursor: 'inherit' } : {};
  return <Dropdown.Toggle {...props} style={{...props.style, ...style}} variant="success" size="sm" />
}

export const PlayMenuItem = (props) => <Dropdown.Item {...props} size="sm">{props.children}</Dropdown.Item>;
