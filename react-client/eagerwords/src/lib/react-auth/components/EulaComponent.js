/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import Modal from "react-bootstrap/Modal";
import Button from "react-bootstrap/Button";

export const EulaComponent = (props) => {
  const {show, closer, canceller, EulaTextComponent, EulaPrefaceComponent, PrivacyComponent} = props;
  const space = ' ';
  let title = 'EagerWords Terms of Use';
  let acceptance = 'I accept the Terms of Use';

  console.log(`EulaComponent - rendering - title: ${title}`);

  // TODO. Can make this a generic modal - ModalPresenterWithCancel.
  // See NotificationComponents.
  return (
    <div>
      <Modal show={show}>
        <Modal.Header>
          <Modal.Title>{title}</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <Button
            variant="primary"
            onClick={() => closer()}
          >
            {acceptance}
          </Button>{space}
          <Button
            variant="primary"
            onClick={() => canceller()}
          >
            Cancel
          </Button>
        </Modal.Body>
        <Modal.Footer>
          <div style={{overflow: 'scroll', height: '400px'}}>
            <p>
              Welcome to EagerWords!
            </p>
            <EulaPrefaceComponent/>
            <EulaTextComponent/>
            <div>{space}</div>
            <PrivacyComponent/>
          </div>
        </Modal.Footer>
      </Modal>
    </div>
  )
};
