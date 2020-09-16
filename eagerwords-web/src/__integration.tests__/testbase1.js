/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {mkPiece} from "../game/domain/Piece";
import {defaultUserGameSettings as settings} from "../game/domain/UserGameSettings";
import {playerTypes} from "../game/domain/GamePlayParams";
import * as PointValue from "../game/domain/PointValue";
import {mkInitPieces} from "../game/domain/InitPieces";
import {errorText} from "../base/util/HttpUtil";
import {mkMovePlayPiece, movedPiecePoints} from "../game/domain/PlayPiece";
import {mkPoint} from "../plane/domain/Point";
import {authService} from '../auth/service/AuthService';
import {getMockEmail, getMockToken} from "../envvars";
import {stringify} from "../base/util/Logger";
import {resultErrorMessage} from "../base/domain/BaseErrors";
import {mkGameParams} from "../game/domain/GameParams";

let mPieces = [mkPiece('S', "4"), mkPiece('T', "5"), mkPiece('Z', "6")];

export const uPieces = [mkPiece('B', "1"), mkPiece('E', "2"), mkPiece('T', "3")];
const dimension = settings.playSettings.dimension;
export const center = parseInt(dimension/2);

let valueFactory = PointValue.mkValueFactory(dimension);
let pointValues = valueFactory.mkEmptyValueGrid();

export const initPieces = mkInitPieces([], uPieces, mPieces);
const playParams = {...settings.playSettings, startingPlayer: playerTypes.machinePlayer};
// export const gameParams = {...settings, startingPlayer: playerTypes.machinePlayer, pointValues};
export const gameParams = mkGameParams(playParams, pointValues);

export const authTester = (function() {
  const mockEmail = getMockEmail();
  const mockToken = getMockToken();
  const loginTimeout = 60; // seconds
  const nickname = "Joe";

  // TODO. URGENT. Move to auth API Utils. Must be consistent with server-side.

  const alreadySignedUpTag = "AlreadySignedUpError";
  const isAlreadySignedUp = (error) => error.tag === alreadySignedUpTag;

  const mockSignUpOrLogin = () => {
    let clientId = null;
    return authService.initSignUp(mockEmail, nickname)
      .then(result => {
        if (result.ok) {
          clientId = result.data.clientId;
          return authService.confirmSignUp(clientId, mockToken)
            .then(result => {
              if (!result.ok)
                console.log(`error in confirmSignUp: ${stringify(result.data)}`);
              expect(result.ok).toBe(true);
              return {
                clientId,
                token: mockToken
              }
            })
        } else {
          expect(isAlreadySignedUp(result.data)).toBe(true);
          return mockLogin();
        }
      })
  };

  const mockLogin = () => {
      let clientId = null;
      return authService.initLogin(mockEmail, loginTimeout)
        .passResult(result => console.log(`vow result: ${stringify(result)}`))
        .then(result => {
          if (!result.ok)
            console.log(`error in initLogin: ${stringify(result.data)}`);
          expect(result.ok).toBe(true);
          clientId = result.data.clientId;
          return authService.confirmLogin(clientId, mockToken)
        }).then(result => {
          if (!result.ok)
            console.log(`error in confirmLogin: ${stringify(result.data)}`);
          expect(result.ok).toBe(true);
          return {
            clientId,
            token: mockToken
          }
        })
        .passResult(result => console.log(`vow result: ${stringify(result)}`))
    };

  return {
    mockSignUpOrLogin,
    mockLogin
  }
})();

export const mkGameTester = gameService => {
  return {

    startGame: () => {
      return gameService.start(gameParams, initPieces)
        .then(result => {
          if (!result.ok)
            console.log(`error in starting game: ${resultErrorMessage(result)}`);
          expect(result.ok).toBe(true);

          let game = result.data;
          expect(game.tray.pieces.length).toBe(settings.trayCapacity);
          expect(game.board.dimension).toBe(settings.dimension);
          console.log("game started");
          return game;
        });
    },

    commitPlay: (gameId) => {
      // Make a horizontal play of BET.
      let playPieces = [
        mkMovePlayPiece(uPieces[0], mkPoint(center, center - 1)),
        mkMovePlayPiece(uPieces[1], mkPoint(center, center)),
        mkMovePlayPiece(uPieces[2], mkPoint(center, center + 1))
      ];

      return gameService.commitUserPlay(gameId, playPieces)
        .then(gameResult => {
          if (!gameResult.ok)
            console.log(`error in committing play: ${resultErrorMessage(result)}`);
          expect(gameResult.ok).toBe(true);
          let {gameMiniState, replacementPieces} = gameResult.data;
          expect(replacementPieces.length).toBe(3);
          expect(gameMiniState.lastPlayScore).toBeGreaterThan(0);
          console.log('committed user play');
          return playPieces;
        });
    },

    machinePlay: (gameId) => {
      return gameService.getMachinePlay(gameId).then(gameResult => {
        if (!gameResult.ok)
          console.log(`error in machine play: ${reqsultErrorMessage(gameResult)}`);
        expect(gameResult.ok).toBe(true);
        let {gameMiniState, playedPieces} = gameResult.data;
        let moves = movedPiecePoints(playedPieces);
        expect(moves.length).toBeGreaterThan(1);
        expect(gameMiniState.lastPlayScore).toBeGreaterThan(0);
        console.log('machine play done');
      });
    },

    closeGame: (gameId) => {
      return gameService.close(gameId).then(result => {
        if (!result.ok)
          console.log(`error in closing game: ${resultErrorMessage(result)}`);
        expect(result.ok).toBe(true);
        // unit response ignore
        console.log("game closed");
      });
    },

    suspendGame: (gameId) => {
      return gameService.suspend(gameId).then(result => {
        if (!result.ok)
          console.log(`error in suspending game: ${resultErrorMessage(result)}`);
        expect(result.ok).toBe(true);
        console.log("game suspended");
        return result.data;
      });
    },

    resumeGame: (gameId) => {
      return gameService.resume(gameId).then(result => {
        if (!result.ok)
          console.log(`error in resuming game: ${resultErrorMessage(result)}`);
        expect(result.ok).toBe(true);
        console.log("game resumed");
        return result.data;
      });
    },

    getFullGame: (gameId) => {
      return gameService.getFullGame(gameId).then(gameResult => {
        if (!gameResult.ok)
          console.log(`error in getting full game: ${resultErrorMessage(gameResult)}`);
        expect(gameResult.ok).toBe(true);
        let game = gameResult.data;
        console.log("got full game");
        return game;
      });

    },
  }
};
