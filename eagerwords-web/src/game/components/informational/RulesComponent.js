/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-table2-paginator/dist/react-bootstrap-table2-paginator.min.css';

export const RulesComponent = (props) => {
    return (
      <div>
        <div style={{fontSize: '35px', color: '#009FFF', textAlign: 'center'}}>Authentication</div>

        <p>
          You may play games as a guest without authentication.
          Guest games are not saved, and cannot be revisited.
        </p>
        <p>
          In order to enjoy the full benefits of the application you are
          encouraged to register with the site. EagerWords uses passwordless
          registration based only on your email address.
        </p>
        <p>
          To register, you provide your email address and a nickname, EagerWords sends a
          security token to your email address, and you complete the registration process
          by providing the token to the application, thus proving that you are the
          legitimate owner of the email. A similar process is used for login.
        </p>
        <div style={{fontSize: '35px', color: '#009FFF', textAlign: 'center'}}>EagerWords Rules</div>

        <p>
          To play, drag and drop letters to form a word using an existing board letter
          (if any) and click <em>Commit Word</em> to play that word.
        </p>
        <ul>
          <li>
            Points are associated with board locations - not with letters.
          </li>
          <li>
            The assignment of points to board locations varies from game to game.
          </li>
          <li>
            The score of a play is the sum of the points associated with the locations
            it captures. Pre-existing letters used in word formation do not contribute
            to the score.
          </li>
          <li>
            A word play on a non-empty board must <em>contain</em> at least one existing letter.
          </li>
          <li>
            A word play on an empty board is unrestricted.
          </li>
          <li>
            The played word and any crosswords formed by the play must exist in the
            game's dictionary.
          </li>
          <li>
            Letters for play are generated randomly based on the frequency of
            letters in the English lexicon as per:
            <a href="http://www.oxfordmathcenter.com/drupal7/node/353">Oxford Math Center</a>.
            Successive letters are generated independently of each other.
            There is no notion of a container of letters getting depleted or letters being
            'used up'.
          </li>
          <li>
            When no board play is possible, a single letter is swapped for another (by dropping it onto the
            <em>Swap Drop</em>).
          </li>
          <li>
            A game ends upon 10 consecutive swaps or when the board is full.
          </li>
          <li>
            If there is no activity on a game for N minutes, the game becomes
            inactive (suspended). A suspended game may be resumed from the <em>Games</em>
            page, which lists the user's games.
          </li>
        </ul>

        <p>
          Note. This game and its rules may evolve over time based on user experience.
        </p>
        <h4>Noteworthy</h4>

        <p>
          You may wish to install a dictionary plugin in your browser to
          easily search for the meanings of words played by the machine.
        </p>
        <p>
          Make sure the status field below the board is visible.
        </p>
        <h4>Configuration</h4>

        There are a number of configuration parameters that you may customize as
        a registered user in the <em>Settings</em> page.

        <ul>
          <li>
            <em>starting player</em> Random (user or machine) by default.
          </li>
          <li>
            <em>board dimension</em>
          </li>
          <li>
            <em>letter tray capacity</em> The number of available letters to each player
            for play at any given time.
          </li>
        </ul>

        <h4>Issues</h4>

        The game's dictionary is based on:

        <p>
          http://www.gutenberg.org/files/3201/files/CROSSWD.TXT -
          a comprehensive list of common English words.
        </p>
        <p>
          But from time to time, you'll run into a common word that is
          not in the dictionary.
        </p>
        <p>
          Please add issues and ideas for improvement (including any missing common words
          in the dictionary) to the
          <a href="https://github.com/azadbolour/eagerwords/issues">issues list.</a>
        </p>


      </div>
    )
};