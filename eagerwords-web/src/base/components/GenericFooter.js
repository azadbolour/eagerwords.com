/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React, { useState } from 'react';
import {stringify} from "../util/Logger";
import {ModalPresenter} from "./NotificationComponents";

const space = <pre> </pre>;

export const GenericFooter = (props) => {

  const [showEula, setShowEula] = useState(false);
  const [showPrivacy, setShowPrivacy] = useState(false);
  const [showNotices, setShowNotices] = useState(false);
  const [showAbout, setShowAbout] = useState(false);

  const {EulaTextComponent, PrivacyComponent, NoticesComponent, AboutComponent} = props;

  const EulaModal = () => {
    let title = "Terms of Service";
    let closer = () => setShowEula(false);
    return (
      <ModalPresenter show={showEula} title={title} closer={closer}>
        <EulaTextComponent/>
      </ModalPresenter>
    )
  };

  const PrivacyModal = () => {
    let title = "Privacy Statement";
    let closer = () => setShowPrivacy(false);
    return (
      <ModalPresenter show={showPrivacy} title={title} closer={closer}>
        <PrivacyComponent/>
      </ModalPresenter>
    )
  };

  const AttributionsModal = () => {
    let title = "Open Source Notices";
    let closer = () => setShowNotices(false);
    return (
      <ModalPresenter show={showNotices} title={title} closer={closer}>
        <NoticesComponent/>
      </ModalPresenter>
    )
  };

  const AboutModal = () => {
    let title = "About EagerWords";
    let closer = () => setShowAbout(false);
    return (
      <ModalPresenter show={showAbout} title={title} closer={closer}>
        <AboutComponent/>
      </ModalPresenter>
    )
  };

  const labelStyle = {color: 'DarkGoldenRod', fontWeight: 'bold', fontSize: 15};
  // TODO. Font color. Perhaps background color?
  return(
    <div>
      <div>{space}</div>
      <div style={{backgroundColor: 'ivory'}}>
        <EulaModal />
        <PrivacyModal />
        <AttributionsModal />
        <AboutModal />

        <a onClick={() => setShowEula(true)} style={labelStyle}>Terms of Service</a> &nbsp;&nbsp;&nbsp;&nbsp;
        <a onClick={() => setShowPrivacy(true)} style={labelStyle}>Privacy Statement</a> &nbsp;&nbsp;&nbsp;&nbsp;
        <a onClick={() => setShowNotices(true)} style={labelStyle}>Attributions</a> &nbsp;&nbsp;&nbsp;&nbsp;
        <a onClick={() => setShowAbout(true)} style={labelStyle}>About</a> &nbsp;&nbsp;&nbsp;&nbsp;
      </div>
    </div>
  );
};
