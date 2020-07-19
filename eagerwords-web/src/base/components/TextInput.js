/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import '../css/App.css';

const TextInput = props => {

  let formControl = "form-control";

  if (props.touched && !props.valid) {
    formControl = 'form-control control-error';
  }

  return (
    <div className="form-group">
      <label>{props.placeholder}</label>
      <span>&nbsp;</span>
      <input type="text" className='form-group-value' {...props}/>
      {props.description}
    </div>
  );
};

export default TextInput;
