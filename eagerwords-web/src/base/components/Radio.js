
import React from 'react';
import '../css/App.css'

const Radio = props => {

  let formControl = "form-control";

  if (props.touched && !props.valid) {
    formControl = 'form-control control-error';
  }

  return (
    <div className="form-group">
      <label>{props.placeholder}</label>
      <div></div>
      {props.options.map(option => (
        <span className="form-group" key={option.value} display="inline">
          <label>{option.displayValue}</label>
          <span>&nbsp;</span>
          <input type="radio"
                 name={props.name}
                 value={option.value}
                 onChange={props.onChange}
                 checked={option.isDefault}
                 className='form-group-value'
          />
          <span>&nbsp;&nbsp;</span>
        </span>
      ))}

    </div>
  );
};

export default Radio;
