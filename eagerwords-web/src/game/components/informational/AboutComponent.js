/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
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
          EagerWords is the second version of the Azad Bolour's crossword game
          featuring a Scala game server and a React UI.
          </p>
        <p>
          This new version provides for passwordless registration
          and authentication via email. Registration allows your
          games to be saved, so that suspended games may resumed at a later time,
          and completed games may be re-examined.
          </p>
        <p>
          Registration also allows you to customize your game preferences, such as
          the size of the board.
        </p>

        <p>
        As a non-registered user you may still play games. But your game cannot be
        saved and restored.
        </p>

        <p>
          EagerWords is <a href="https://github.com/azadbolour/eagerwords">open sourced</a>
          under AGPL.
        </p>
        <div style={{padding: 10}}>
          <img src="/agplv3-88x31.png" alt="image" />
        </div>

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


