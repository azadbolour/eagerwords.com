/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

// TODO. Make this function generic and reusable by any components.
import Button from "react-bootstrap/Button";
import React from "react";
import Alert from "react-bootstrap/Alert";
import Spinner from "react-bootstrap/Spinner";
import Modal from "react-bootstrap/Modal";
// import ModalBody from "react-bootstrap/ModalBody";
// import ModalHeader from "react-bootstrap/ModalHeader";
// import ModalFooter from "react-bootstrap/ModalFooter";
// import ModalTitle from "react-bootstrap/ModalTitle";

export const systemError = message => `We are sorry - eagerwords has encountered a system error and must be reset. ${message} `;
export const unrecoverableResetButtonTestId = "unrecoverableResetButton";
export const loginExpiredResetButtonTestId = "loginExpiredResetButton";

/**
 * Generic alerter.
 * @param props
 *  alert - alert JSX
 *  reset - callback to cleanup the alert status
 */
export const Alerter = (props) => {
  let alert = props.alert;
  let reset = props.reset;
  // TODO. Reserve a fixed height div for the alert area to prevent relocations.
  if (alert === null)
    return (<div></div>);
  return(
    <div>
      <Alert onClose={() => reset()} show={true} variant='success' closeLabel='OK' dismissible fade="false">
        {alert}
      </Alert>
    </div>
  )
};

// TODO. Remove. No longer used,
export const MessageArea = (props) => {
  let message = props.message;
  if (!message)
    return (<div></div>);

  return (
    <div style={{color: 'Red', backgroundColor: 'LightGrey'}}>
      {message}
    </div>
  );
};

/**
 * Notify that user is not logged in.
 *
 * @param props
 *    show - to display or not
 *    message - the notification message.
 *    reset - callback to allow login.
 */
export const LoggedOutModal = (props) => {
  let show = props.show;
  let title = "Not Logged In!";
  let message = props.message;
  let label = "Reset";
  let buttonTestId = loginExpiredResetButtonTestId;
  let onClick = props.reset;

  return(
    <div>
      <ModalNotifier
        buttonTestId={buttonTestId}
        show={show}
        title={title}
        message={message}
        label={label}
        onClick={onClick}
      />
    </div>
  )
};

/**
 * Notify that unrecoverable error has occurred.
 *
 * @param props
 *  reset - callback to return to safety.
 */
export const UnrecoverableModal = (props) => {
  let show = props.show;
  let title = "Error!";
  let message = props.message;
  let label = "Reset";
  let buttonTestId = unrecoverableResetButtonTestId;
  let onClick = props.reset;

  return(
    <div>
      <ModalNotifier
        buttonTestId={buttonTestId}
        show={show}
        title={title}
        message={message}
        label={label}
        onClick={onClick}
      />
    </div>
  )
};

export const InvalidInputModal = (props) => {
  const {show, closer, children} = props;
  let title = "Invalid Input";

  return(
    <div>
      <ModalPresenter
        show={show}
        title={title}
        label="OK"
        closer={closer}
      >
        {children}
      </ModalPresenter>
    </div>
  )
};

/**
 * Notify waiting for an action to complete.
 *
 * @param props
 *  message - the processing message to appear with the spinner.
 */
export const ProcessingSpinner = (props) => {
  let message = props.message;
  let show = props.show;
  if (!show)
    return (<div></div>);

  return (
    <div>
      <Button variant="primary" disabled>
        <Spinner
          as="span"
          animation="border"
          size="sm"
          role="status"
          aria-hidden="true"
        />
        <span className='ml-2'>{message}</span>
      </Button>{' '}
    </div>)
};

export const ProcessingSpinnerModal = (props) => {
  let message = props.message;
  let show = props.show;
  if (!show)
    return (<div></div>);

  return (
    <div>
      <Modal show={show}>
        <Modal.Body>
          <Button variant="primary" disabled>
            <Spinner
              as="span"
              animation="border"
              size="sm"
              role="status"
              aria-hidden="true"
            />
            <span className='ml-2'>{message}</span>
          </Button>{' '}
        </Modal.Body>
      </Modal>
    </div>)
};

export const ModalNotifier = (props) => {
  let show = props.show;
  let title = props.title;
  let message = props.message;
  let label = props.label;
  let buttonTestId = props.buttonTestId;
  let onClick = () => {
    console.log(`ModalNotifier - onClick called`);
    props.onClick();
  };

  if (!show)
    return (<div></div>);

  return (
    <div>
      <Modal show={show}>
        <Modal.Header>
          <Modal.Title>{title}</Modal.Title>
        </Modal.Header>
        <Modal.Body>{message}</Modal.Body>
        <Modal.Footer>
          <Button
            data-testid={buttonTestId}
            variant="primary"
            onClick={() => onClick()}
          >
            {label}
          </Button>
        </Modal.Footer>
      </Modal>
    </div>
  )
};

// TODO. Maximum height of the body of the modal should be a prop.

export const ModalPresenter = (props) => {
  const {show, title, label, closer, children} = props;
  let theLabel = label ? label : 'OK';

  return (
    <div>
      <Modal show={show}>
        <Modal.Header>
          <Modal.Title>{title}</Modal.Title>
        </Modal.Header>
        <Modal.Body style={{maxHeight: '200px', overflowY: 'auto'}}>{children}</Modal.Body>
        <Modal.Footer>
          <Button
            variant="primary"
            onClick={() => closer()}
          >
            {theLabel}
          </Button>
        </Modal.Footer>
      </Modal>
    </div>
  )
};




