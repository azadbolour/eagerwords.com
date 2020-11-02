/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {FormCheck} from "react-bootstrap";
import React from "react";

import {
  setOpMockLoggedOut,
  setOpMockTimeout,
  setOpMockError, opMockError, opMockTimeout, opMockLoggedOut,
} from './ComponentOperationState';

/*
 * Generic service-call mocking infrastructure for components.
 *
 * To allow testers to optionally mock different types of errors:
 *
 * 1. Make sure the component includes state variables for mocking,
 *    as defined in ApiAdapters.defaultMockErrors.
 *
 * 2. Wrap the component in a ServiceProcessingDecorator and
 *    provide a testingControls prop to the decorator as described
 *    below for TestingControls. This will provide checkboxes
 *    in the component for testers to check to simulate different
 *    types of errors (see below for teh definition of the checkboxes).
 *
 * 3. When making service calls from the component, call
 *    opMockEffects(component) to obtain a data structure that
 *    describes what to mock, and pass that data structure to the
 *    service call as its last argument.
 *
 * 4. In the service layer, have the calls to the API layer be
 *    intercepted ApiAdapters.apiMockErrorAdapter to simulate
 *    errors.
 */

export const unrecoverableCheckboxTestId = "unrecoverableCheckbox";
export const timeoutCheckboxTestId = "timeoutCheckbox";

export const ForceUnrecoverableCheckBox = (props) => {
  let checked = props.getCheckedState();
  let onChange = (evt) => {
    let checked = evt.target.checked;
    console.log(`unrecoverable checkbox onChange: checked: ${checked}`);
    props.setCheckedState(checked);
  };
  return (<FormCheck
    data-testid={unrecoverableCheckboxTestId}
    id='checkSignInUnrecoverable'
    label='force unrecoverable error'
    type='checkbox'
    onChange={(evt) => onChange(evt)}
    checked={checked}
  />)
};

export const UnrecoverableCheckBox = (props) =>
  <ForceUnrecoverableCheckBox
    setCheckedState={(checked) => setOpMockError(props.comp, checked)}
    getCheckedState={() => opMockError(props.comp)}
  />;


export const ForceTimeoutCheckBox = (props) => {
  let checked = props.getCheckedState();
  let onChange = (evt) => {
    let checked = evt.target.checked;
    props.setCheckedState(checked);
  };
  return (
    <FormCheck
      data-testid={timeoutCheckboxTestId}
      id='checkSignInTimeout'
      label='force timeout'
      type='checkbox'
      onChange={(evt) => onChange(evt)}
      checked={checked}
    />
  )
};

export const TimeoutCheckBox = (props) =>
  <ForceTimeoutCheckBox component={this}
    setCheckedState={(checked) => setOpMockTimeout(props.comp, checked)}
    getCheckedState={() => opMockTimeout(props.comp)}
  />;

export const ForceLoginExpiredCheckBox = (props) => {
  let checked = props.getCheckedState();
  let onChange = (evt) => {
    let checked = evt.target.checked;
    props.setCheckedState(checked);
  };
  return (
    <FormCheck
      id='checkSignInTimeout'
      label='force expired login'
      type='checkbox'
      onChange={(evt) => onChange(evt)}
      checked={checked}
    />
  )
};

export const LoginExpiredCheckBox = (props) =>
  <ForceLoginExpiredCheckBox component={this}
    setCheckedState={(checked) => setOpMockLoggedOut(props.comp, checked)}
    getCheckedState={() => opMockLoggedOut(props.comp)}
  />;

export const testingControlName = {
  unrecoverable: 'unrecoverable',
  timeout: 'timeout',
  loginExpired: 'loginExpired',
};

const testingControlMap = (comp) => {return {
  [testingControlName.unrecoverable]: <UnrecoverableCheckBox comp={comp} />,
  [testingControlName.timeout]: <TimeoutCheckBox comp={comp} />,
  [testingControlName.loginExpired]: <LoginExpiredCheckBox comp={comp} />,
}};

/**
 * Generic mocking UI for different types of errors.
 *
 * The ServiceProcessingDecorator that is used to wrap components with
 * processing and error displays may optionally include a TestingControls.
 *
 * TestingControls provides checkboxes for mocking different types of errors.
 * The types are defined above in testingControlNames.
 *
 * Checking the appropriate checkbox causes a callback to be invoked on the
 * component to set a component state variable that informs service-level actions
 * started from the component to mock the corresponding error.
 *
 * See above for the specifications of the mock error checkboxes.
 * For each checkbox, the setCheckedState prop sets the corresponding
 * mock error state of the component, and the getCheckedState gets that state.
 *
 * @param props
 *    props.comp The component that includes the TestingControls.
 *    props.testingControls A list of names of the error types to allow to be mocked.
 *                          Checkboxes will be provided for these error types.
 */
export const TestingControls = (props) => {
  let comp = props.comp;
  let controlNames = props.controls;
  // let testingControls = controlNames.map(name => testingControlMap(comp)[name]);
  return (
    <div style={{display: 'flex', flexDirection: 'column'}}>
      {controlNames.includes(testingControlName.unrecoverable) && <UnrecoverableCheckBox comp={comp}/>}
      {controlNames.includes(testingControlName.timeout) && <TimeoutCheckBox comp={comp}/>}
      {controlNames.includes(testingControlName.loginExpired) && <LoginExpiredCheckBox comp={comp}/>}
    </div>
  )
};


