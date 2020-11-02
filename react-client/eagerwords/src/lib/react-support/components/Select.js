import React from 'react';
import '../css/App.css'

const Select = props => {

  let formControl = "form-control";

  if (props.touched && !props.valid) {
    formControl = 'form-control control-error';
  }

  return (
    <div className="form-group">
      <label>{props.placeholder}</label>
      <span>&nbsp;</span>
      <select className='form-group-value' value={props.value} onChange={props.onChange} name={props.name}>
        {props.options.map(option => (
          <option value={option.value} key={option.value} className='form-group-value'>
            {option.displayValue}
          </option>
        ))}
      </select>
    </div>
  );
};

export default Select;
