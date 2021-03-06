/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-table2-paginator/dist/react-bootstrap-table2-paginator.min.css';
import {Header} from "semantic-ui-react";

export const AboutComponent = (props) => {
  return(
    <div>
      <Header as="h5" align='center' style={{color: 'Green'}}>About EagerWords</Header>
      <div>
        <p>
          EagerWords is the second version of the Azad Bolour's crossword game.
          It features a Scala game server and a React web UI.
          </p>
        <p>
          EagerWords supports user registration and login by using email-based
          passwordless authentication. Registering as a user allows your
          games to be saved, so that suspended games may resumed at a later time,
          and completed games may be re-examined.
        </p>
        <p>
          Registration also allows you to customize your game preferences, such as
          the size of the board.
        </p>

        <p>
        As a non-registered user you may still play games. But your games cannot be
        saved and restored.
        </p>

        <p>
          EagerWords is <a href="https://github.com/azadbolour/eagerwords.com" target="_blank" rel="noopener noreferrer">open sourced</a> under AGPL.
        </p>
        <div style={{padding: 10}}>
          <img src="/agplv3-88x31.png" alt="image" />
        </div>

        <p>
          Attributions to the open source dependencies of EagerWords are listed in
          <a href="https://github.com/azadbolour/eagerwords.com/blob/master/NOTICES.txt">open source notices</a>.
        </p>

      </div>

      <div style={{color: '#009FFF', fontSize: '20px'}}>
        Bolour Computing.
        <br/>
        6144 N Rockridge Blvd
        <br/>
        Oakland, CA 94618
      </div>
    </div>
  )
};


