/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

export const disabledColor = '#B0B0B0';
export const disabledBackgroundColor = '#D0D0D0';

export const buttonStyle = function(enabled) {
  let color = enabled ? 'Chocolate' : disabledColor;
  let backgroundColor = enabled ? 'Khaki' : disabledBackgroundColor;
  return {
    width: '120px',
    height: '40px',
    color: color,
    backgroundColor: backgroundColor,
    align: 'left',
    fontSize: 15,
    fontWeight: 'bold',
    borderWidth: '4px',
    margin: 'auto',
    padding: '2px'
  }
};
