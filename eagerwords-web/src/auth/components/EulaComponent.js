/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React from 'react';
import Modal from "react-bootstrap/Modal";
import Button from "react-bootstrap/Button";

export const EulaComponent = (props) => {
  const {show, closer, canceller, EulaTextComponent} = props;

  let title = 'EagerWords Terms of Service';
  let acceptance = 'I accept the Terms of Service';

  // TODO. Can make this a generic modal - ModalPresenterWithCancel.
  // See NotificationComponents.
  return (
    <div>
      <Modal show={show}>
        <Modal.Header>
          <Modal.Title>{title}</Modal.Title>
        </Modal.Header>
        <Modal.Body>
            <EulaTextComponent/>
        </Modal.Body>
        <Modal.Footer>
          <Button
            variant="primary"
            onClick={() => closer()}
          >
            {acceptance}
          </Button>
          <Button
            variant="primary"
            onClick={() => canceller()}
          >
            Cancel
          </Button>
        </Modal.Footer>
      </Modal>
    </div>
  )
};