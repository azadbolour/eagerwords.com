
/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {PlayerType} from "../api/PlayerType";

export const isSwapPlay = (play) => play && play.word.length === 0;
export const isWordPlay = (play) => play && play.word.length > 0;
export const isMachinePlay = (play) => play && play.playerType === PlayerType.machine;
export const isUserPlay = (play) => play && play.playerType === PlayerType.user;

/**
 * Evolution of plays in a game.
 */
export const mkPlayHistory = function(wordsPlayed) {
  let _wordsPlayed = wordsPlayed;

  return {
    get wordsPlayed() { return _wordsPlayed; },
    get lastPlay() {return _wordsPlayed.slice(-1)[0]}, // Undefined for an empty list!
    get machinePlayedLast() {return isMachinePlay(this.lastPlay)},
    get lastPlayIsSwap() {return isSwapPlay(this.lastPlay)},

    // TODO. URGENT. Change playerName to playerType. "User", "Machine".

    pushWordPlayed(word, playerType) {
      let wordPlayed = {
        word: word,
        playerType: playerType
      };
      return mkPlayHistory([..._wordsPlayed, wordPlayed]);
    },
  }
};


export const emptyPlayHistory = function() {return mkPlayHistory([])};
