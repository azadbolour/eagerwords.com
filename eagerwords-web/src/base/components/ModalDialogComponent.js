/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React, { Component } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-table2-paginator/dist/react-bootstrap-table2-paginator.min.css';
import PropTypes from "prop-types";
import Button from "react-bootstrap/Button";
import Modal from 'react-modal';
import {stringify} from "../util/Logger";

const Main = ({children}) => <div style={{
  height: '90%',
  overflowY: 'auto'
}}>
  {children}
</div>;

const Bottom = ({children}) => <div style={{
  height: '10%',
  textAlign: 'center'
}}>
  {children}
</div>;

class ModalDialogComponent extends Component {
  static propTypes = {
    contents: PropTypes.func.isRequired,
    isOpen: PropTypes.bool.isRequired,
    close: PropTypes.func.isRequired
  };

  render() {
    const {props} = this;
    const {contents: Contents, close, isOpen} = props;

    console.log(`isOpen: ${stringify(isOpen)}`);

    return <div>
      <Modal
        isOpen={isOpen}
        onRequestClose={close}
        contentLabel='Modal Dialog'
      >
        <div style={{overflowY: 'auto'}}>
          <Contents/>
        </div>
        <div style={{textAlign: 'center'}}>
          <Button onClick={close}>
            OK
          </Button>
        </div>
      </Modal>

    </div>
  }
}

export default ModalDialogComponent;
