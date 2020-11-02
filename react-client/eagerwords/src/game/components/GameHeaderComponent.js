
import React, { useState } from 'react';
import ButtonToolbar from "react-bootstrap/ButtonToolbar";
import PropTypes from "prop-types";
import {authService} from "lib/react-auth/index";
import ButtonBase from "react-bootstrap/Button";
import Dropdown from "react-bootstrap/Dropdown";

// TODO. Duplicated from PlayComponent. Remove duplicate code.
function Button(props) {
  let disabled = props.disabled;
  // override bootstrap's 'pointer' cursor if the button is disabled.
  let style = disabled ? { cursor: 'inherit' } : {};
  return <ButtonBase {...props} style={{...props.style, ...style}}/>
}

const space = <pre> </pre>;

export const GameHeader = (props) => {
  const {loginEvidence, onLogout, onUnregisterUser, helpUrl} = props;
  const clientId = loginEvidence ? loginEvidence.clientId : null;
  const token = loginEvidence ? loginEvidence.token : null;
  let loggedIn = clientId !== null && token !== null;

  // TODO. URGENT. Standard error processing.
  let doLogout = () => {
    authService.logout(clientId, token).then( () => {
      onLogout();
    })
  };

  let doUnregister = () => onUnregisterUser();

  return (
    <div style={{display: 'flex', flexDirection: 'row'}}>
      <ButtonToolbar aria-label="header">
        <span style={{width: '540px'}}/>
        <Dropdown>
          <Dropdown.Toggle variant="success" size="sm" >Admin</Dropdown.Toggle>
          <Dropdown.Menu>
            <Dropdown.Item variant="success" size="sm" onClick={() => doUnregister()}>Unregister</Dropdown.Item>
          </Dropdown.Menu>
        </Dropdown>{space}
        <Button disabled={!loggedIn} size="sm" variant="success" onClick={() => doLogout()}>Logout</Button>{space}
        <Button size="sm" variant="success">
        <a href={helpUrl} target="_blank" rel="noopener noreferrer" style={{color: 'white'}}>Help</a>
        </Button>
        { /* <Button size="sm" variant="success" onClick={() => onHelp()}>Help</Button>{space} */ }
      </ButtonToolbar>
    </div>
  );
};

GameHeader.propTypes = {
  loginEvidence: PropTypes.object,
  onLogout: PropTypes.func.isRequired
};
