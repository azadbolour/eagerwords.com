/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React from 'react';
import '../css/App.css';

const BareTextInput = props => {

  let formControl = "form-control";

  if (props.touched && !props.valid) {
    formControl = 'form-control control-error';
  }

  return (
    <div className="form-group">
      <input type="text" className='form-group-value' {...props}/>
      {props.description}
      <span style={{width: '10px'}}>&nbsp;&nbsp;&nbsp;</span>
      <label style={{color: "red", fontWeight: "bold"}}>{props.message}</label>
    </div>
  );
};

export default BareTextInput;