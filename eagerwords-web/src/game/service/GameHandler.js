 /*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {stringify} from "../../base/util/Logger";
import {mkInitPieces} from '../domain/InitPieces';
import GameService from "./GameService";
import {playerStarts} from "../domain/GameParams";
import {errorTags, mkUiErrorResult} from "../domain/GameErrors";
import {resolveVow} from "../../base/domain/Vow";
import {valueResult} from "../../base/domain/Result";

// TODO. Update the doc comments below to reflect refactoring to use Vow.
// Fetch catches are converted to Result errors at the service layer.
// Other catches will only be caused by bugs and will bubble up to the error boundary.

 // TODO. URGENT. These should be aggregated in an object.

export const startGameDisplay = 'starting game';
export const getUserGamesDisplay = 'getting user games';
export const unregisterUserDisplay = 'unregistering user';
export const resumeDisplay = 'resuming game';
export const getUserGameSettingsDisplay = 'getting game settings';
export const saveUserGameSettingsDisplay = 'saving game settings';
export const getFullGameDisplay = 'getting game';
export const commitPlayDisplay = 'committing play';
export const swapDisplay = 'swapping piece';
export const suspendDisplay = 'suspending game';
export const cancelDisplay = 'cancelling game';
export const resignDisplay = 'resigning game';

/**
 * High level service interface for manipulating game data including storage and retrieval
 * in the backend game server. Its reason for existence is to implement complex functions
 * needed by components that require multiple calls to the service layer plus some business
 * logic independent of UI concerns. However for a clean layering structure, all functions
 * of the service layer needed by components are accessed through this single access point.
 */
export const mkGameHandler = function(maybeLoginEvidence) {
  const _gameService = new GameService(maybeLoginEvidence);

  let handler = {
    /**
     * Starts a game based on the input game parameters. Among other things, the starting
     * player is specified in the input parameters. If the starting player is the machine,
     * calls the backed to get the first play and reflect on the board.
     *
     * A top-level method called when the user requests to start a new game.
     *
     * @param gameParams - Properties of the game to be started.
     */
    start: function(gameParams) {
      let started = this.startInternal(gameParams);
      if (playerStarts(gameParams))
        return started;
      return started
        .flatMap(this.machinePlayInternal)
        .mapValue(({game}) => game);
    },

    checkRunning: function(game, actionName) {
      let ok = game.running();
      let data = ok ? game : mkUiErrorResult(errorTags.terminatedGame);
      return resolveVow({ok, data});
    },

    /**
     * Commit the user's current play and get the next machine play from
     * the backend. Returns a vow of the updated game state.
     *
     * A top-level call when the user commits a play.
     */
    commitPlayAndGetMachinePlay: function(game) {
      return this.checkRunning(game, 'commit')
        .flatMap(this.commitPlayInternal)
        .flatMap(({game: committedGame, noMorePlays}) => {
          return this.completeUserPlay(committedGame, noMorePlays);
        })
    },

    completeUserPlay: function(game, finished) {
      return finished ? this.closeInternal(game) : this.machinePlay(game)
    },

    machinePlay: function(game) {
      return this.machinePlayInternal(game)
        .flatMap(({game, gameMiniState}) => {
          return this.completeMachinePlay(game, gameMiniState)
        })
    },

    /**
     * Swap a user piece for another piece allocated by the system and
     * get the next machine move from the system.
     *
     * @param piece The tray piece to be swapped.
     */
    swapAndGetMachinePlay: function(game, piece) {
      return this.checkRunning(game, 'swap')
        .flatMap(game => {
          return this.handleSwapInternal(game, piece);
        })
        .flatMap(({game: updatedGame, noMorePlays}) => {
          return this.completeUserPlay(updatedGame, noMorePlays);
        });
    },

    suspend: function(gameId) {
      return _gameService.suspend(gameId);
    },

    resume: function(gameId) {
      return _gameService.resume(gameId);
    },

    cancel: function(gameId) {
      return _gameService.cancel(gameId);
    },

    resign: function(gameId) {
      return _gameService.resign(gameId);
    },

    getUserGames: function(...args) {
      return _gameService.getUserGames(...args);
    },

    /**
     * Get all the information about a game - used to resume a suspended game.
     */
    getFullGame: function(gameId) {
      return _gameService.getFullGame(gameId);
    },

    saveUserGameSettings: function(settings) {
      return _gameService.saveUserGameSettings(settings);
    },

    getUserGameSettings: function() {
      return _gameService.getUserGameSettings();
    },

    unregisterUser: function() {
      return _gameService.unregisterUser();
    },

    /**
     * Start a game based on the input parameters without doing any plays, and
     * return the game state for the started game.
     */
    startInternal: function(gameParams) {
      let initPieces = mkInitPieces([], [], []);
      return _gameService.start(gameParams, initPieces);
    },

    commitPlayInternal: function(game) {
      // TODO. Refactoring this block to a method results in a so far undiagnosed error!
      let playPiecesResult;
      try {
        // TODO. Maybe this will avoid the error.
        // TODO. Instead of throw, this function could just return a Result.
        let playPieces = game.getCompletedPlayPieces();
        playPiecesResult = valueResult(playPieces);
      } catch (errorTag) {
        playPiecesResult = mkUiErrorResult(errorTag);
      }

      // console.log(`${stringify(playPiecesResult)}`);

      let playPieces = undefined;
      return resolveVow(playPiecesResult)
        .flatMap($playPieces => {
          playPieces = $playPieces;
          return _gameService.commitUserPlay(game.gameId, $playPieces);
        })
        .mapValue(({gameMiniState: {lastPlayScore, noMorePlays}, replacementPieces, deadPoints}) => {
          let updatedGame = game.commitUserMoves(playPieces, lastPlayScore, replacementPieces, deadPoints);
          return {game: updatedGame, noMorePlays};
        });
    },

    /**
     * Get a machine play from the system and return a Vow of data about it:
     */
    machinePlayInternal: function(game) {
      return _gameService.getMachinePlay(game.gameId)
        .mapValue(({gameMiniState, playedPieces, deadPoints}) => {
          let updatedGame = game.commitMachineMoves(gameMiniState.lastPlayScore, playedPieces, deadPoints);
          return {game: updatedGame, gameMiniState};
        });
    },

    /**
     * A machine play has been executed; complete its processing by ending
     * the game if the backend has declared the game as ended (timed out).
     *
     * @param machinePlayResult Vow of result of the the machine play.
     */
    // TODO. URGENT. Fold this function into its only user: MachinePlay.
    completeMachinePlay: function(game, gameMiniState) {
      let shouldClose = gameMiniState.noMorePlays;
      if (!shouldClose)
        return resolveVow({ok: true, data: game});
      return this.closeInternal(game);
    },

    /**
     * The server has indicated that the game should end - so go ahead and end it,
     * returning a vow of its result.
     */
    closeInternal: function (game) {
      return _gameService.close(game.gameId)
        .mapValue(({stopInfo}) => game.end(stopInfo));
    },

    /**
     * Swap a piece and return a vow of its result.
     *
     * @param pc The tray piece to be swapped.
     */
    handleSwapInternal: function (game, pc) {
      return _gameService.swap(game.gameId, pc)
        .mapValue(({gameMiniState: {noMorePlays}, piece}) => {
          const updatedGame = game.swapTrayPiece(pc.id, piece);
          return {game: updatedGame, noMorePlays};
        });
    }
  };
  return handler;
};
