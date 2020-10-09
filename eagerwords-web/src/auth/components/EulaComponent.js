/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import Modal from "react-bootstrap/Modal";
import Button from "react-bootstrap/Button";
import {PrivacyComponent} from "../../game/components/informational/PrivacyComponent";

export const EulaComponent = (props) => {
  const {show, closer, canceller, EulaTextComponent} = props;
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
            <p>
            This <em>Terms of Use</em> and <em>Privacy Policy</em> (see below)
            constitute an agreement (“Agreement”) between you and Bolour Computing, its subsidiaries,
            affiliates, and third-party associates (“EagerWords”).
            </p>
            <EulaTextComponent/>
            <div>{space}</div>
            <PrivacyComponent/>
          </div>
        </Modal.Footer>
      </Modal>
    </div>
  )
};
