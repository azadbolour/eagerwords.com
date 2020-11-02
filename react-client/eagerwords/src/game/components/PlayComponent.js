/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {push} from "connected-react-router";
import PropTypes from 'prop-types';
import React from 'react';
import Button from "react-bootstrap/Button";
import ButtonToolbar from "react-bootstrap/ButtonToolbar";
import Dropdown from "react-bootstrap/Dropdown";
import {DndProvider} from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import {default as TouchBackend} from 'react-dnd-touch-backend';
import ReactList from 'react-list';
import {connect} from 'react-redux';
import {withRouter} from "react-router";
import {createStructuredSelector} from 'reselect';
import {authRoutingPaths} from "lib/react-auth/index";
// import {loggedOut} from "../../auth/redux/AuthActions";
import {AuthActions} from 'lib/react-auth/index';

// import {selectIsGuest, selectLoginEvidence, selectNickname} from "../../auth/redux/AuthSelector"
import {AuthSelector} from 'lib/react-auth/index';
import {authService} from "lib/react-auth/index";

import {ComponentOperationState} from 'lib/react-support/index';
import {checkLoggedIn} from 'lib/react-auth/index';
import {DeviceTypes} from "lib/js-util/index";
import {selectServerInfo} from "../../game/redux/gameSelector";
import {BrowserUtil} from "lib/js-util/index";
import {stringify} from 'lib/js-util/index';
import * as styles from '../css/PlayStyles';
import {PlayButton, PlayDropdownToggle, PlayMenuItem} from '../css/PlayStyles';
import * as Game from '../domain/Game';
import {logPostMortemGame, noGame} from '../domain/Game';
import {machineStarts, mkGameParams} from "../domain/GameParams";
import {fixStartingPlayer,} from '../domain/GamePlayParams';
import * as PointValue from "../domain/PointValue";
import {defaultUserGameSettings} from '../domain/UserGameSettings';
import {gameSelectedForResumption,} from "../redux/GameActions";
import {selectGameHandler, selectGameIdToResume,} from "../redux/gameSelector";

import {
  cancelDisplay,
  commitPlayDisplay,
  getFullGameDisplay,
  getUserGameSettingsDisplay,
  resignDisplay,
  startGameDisplay,
  swapDisplay
} from "../service/GameHandler";
import BoardComponent from './BoardComponent';
import {rulesHtmlName} from "./GameComponentConstants";
import {gameRoutingPaths} from "./GameRoutingPaths";
import SwapBinComponent from './SwapBinComponent';
import TrayComponent from './TrayComponent';
import {VowModule} from 'lib/js-util/index';
import {ComponentProcessingStateManager} from 'lib/react-support/index';

let {selectIsGuest, selectLoginEvidence, selectNickname} = AuthSelector;
let {loggedOut} = AuthActions;
let {
  serviceStateSettingInterceptor,
  ServiceProcessingDecorator
} = ComponentProcessingStateManager;
let {defaultOpContext, setOpAlertMsg} = ComponentOperationState;
let {valueVow} = VowModule;
let {deviceTypes} = DeviceTypes;
let {inputDeviceInfo, getUrlOfPublicResource} = BrowserUtil;

/*
 * Note. Do not use &nbsp; for spaces in JSX. It sometimes comes out as circumflex A
 * (for example in some cases with the Haskell Servant web server). Have not been
 * able to figure why and in what contexts, or how much of it is a React issue and
 * how much Servant. Use <pre> </pre> for spaces instead.
*/
const space = <pre> </pre>;

const copyright = 'Copyright 2017-2020 Azad Bolour.';

const gameSummaryMessage = function(game) {
  let stopInfo = game.stopInfo;
  if (!stopInfo)
    return '';
  let {successivePasses, filledBoard} = stopInfo;
  let message = "game over - ";
  message += filledBoard ? `full board` : `${successivePasses} successive passes - maxed out`;
  return message;
};

const infoStatus = function(game) {
  if (game.finished())
    return gameSummaryMessage(game);
  let plays = game.plays;
  let gameParams = game.gameParams;
  let justStarted = plays.wordsPlayed.length <= 1;
  let machineStarted = machineStarts(game.gameParams);
  let machinePassed = plays.machinePlayedLast && plays.lastPlayIsSwap;

  if (justStarted && machinePassed)
    return "game started - bot passed - your turn ...";
  if (justStarted)
    return machineStarted ? "game started by bot - your turn ..." : "game started - your turn ...";
  if (machinePassed)
    return 'bot took a pass - your turn ...';
  return "your turn ...";
};

const rulesPublicUrl = getUrlOfPublicResource(rulesHtmlName);

/**
 * The entire game UI components including the board and game buttons.
 */
class PlayComponent extends React.Component {

  constructor(props) {
    super(props);

    // Set the 'this' in callbacks to be this component.
    this.onMove = this.onMove.bind(this);
    this.onRevertMove = this.onRevertMove.bind(this);
    this.onSwap = this.onSwap.bind(this);
    this.commitPlay = this.commitPlay.bind(this);
    this.revertPlay = this.revertPlay.bind(this);
    this.suspendGame = this.suspendGame.bind(this);
    this.resignGame = this.resignGame.bind(this);
    this.cancelGame = this.cancelGame.bind(this);

    this.authService = authService;

    this.state = {
      opContext: defaultOpContext,
      game: null,
      userGameSettings: null,
      dndBackend: HTML5Backend
    };

    this.wordsComponent = null;
  }

  static propTypes = {
    gameHandler: PropTypes.object.isRequired,
    serverInfo: PropTypes.object.isRequired,
  };

  async componentDidMount() {
    // console.log(`PlayComponent - in componentDidMount`);
    if (this.props.isGuest) {
      this.startGame();
      return;
    }

    let loggedIn = await checkLoggedIn(this);
    // console.log(`messages: ${stringify(this.state.opContext.messages)}`);
    // console.log(`loggedIn: ${stringify(loggedIn)}`);

    if (!loggedIn)
      return;

    let gameIdToResume = this.props.gameIdToResume;
    if (gameIdToResume === null) {
      this.startGame();
      return;
    }

    this.props.resetResumedGameId();
    this.resumeGame(gameIdToResume);
  }

  async initializeUserGameSettings() {
    let handler = this.props.gameHandler;
    let vow = this.props.isGuest ?
      valueVow(defaultUserGameSettings) :
      serviceStateSettingInterceptor(this, getUserGameSettingsDisplay, handler, handler.getUserGameSettings);
    let result = await vow.unwrap;
    let userGameSettings = result.ok ? result.data : null;
    if (userGameSettings === null)
      userGameSettings = defaultUserGameSettings;

    const preferredDevice = userGameSettings.lookAndFeelSettings.preferredDevice;
    let {device} = inputDeviceInfo(preferredDevice);
    let dndBackend = device === deviceTypes.touch ? TouchBackend : HTML5Backend;
    this.setState((state) => {return {...state, dndBackend, userGameSettings}});
    console.log(`userGameSettings: ${stringify(userGameSettings)}`);
  }

  resumeGame(gameId) {
    return this.initializeUserGameSettings().then(() => {
      let handler = this.props.gameHandler;
      serviceStateSettingInterceptor(this, getFullGameDisplay, handler, handler.getFullGame, gameId).passValue(
        (game) => {
          this.setStateGame(game);
          this.scrollToLastWord();
        }
      )
    })
  }

  componentDidUpdate() {
    this.scrollToLastWord();
  }

  gameMethod() {
    let game = this.game();
    return {
      canMovePiece: game.canMovePiece.bind(game),
      legalMove: game.legalMove.bind(game),
      isTrayPiece: game.tray.isTrayPiece.bind(game.tray)
    }
  };

  setStateGame(game) {
    // let gameSettings = game ? game.gameParams : null;
    this.setState((state) => {return {...state, game}});
  }

  setStateInfoMessage(customMessage) {
    setOpAlertMsg(this, customMessage);
  }

  game() {
    return this.state.game;
  }

  gamePlays() {
    return this.game().plays;
  }

  wordsPlayed() {
    return this.gamePlays().wordsPlayed;
  }

  StatusAsButton = (props) => {
    const errorStatus = this.state.opContext.messages.alertMsg;
    let style = {
      color: 'White',
      backgroundColor: 'DarkGoldenRod',
      borderColor: 'DarkGoldenRod'
    };
    let status = errorStatus ? errorStatus : infoStatus(this.game());
    return (
      <div style={{marginBottom: '5px'}}>
        <Button size="sm" style={style} >{status}</Button>
      </div>
    )
  };

  commitPlay() {
    let handler = this.props.gameHandler;
    serviceStateSettingInterceptor(this, commitPlayDisplay, handler, handler.commitPlayAndGetMachinePlay, this.game()).passValue(
      ((game) => this.setStateGame(game))
    )
  }

  async suspendGame() {
    const gameId = this.game().gameId;
    // let handler = this.props.gameHandler;
    // let suspendVow = serviceStateSettingInterceptor(this, suspendDisplay, handler, handler.suspend, gameId);
    // await suspendVow.unwrap;
    this.props.onGameSuspended(gameId);
  }

  async cancelGame() {
    const gameId = this.game().gameId;
    let handler = this.props.gameHandler;
    let cancelVow = serviceStateSettingInterceptor(this, cancelDisplay, handler, handler.cancel, gameId);
    await cancelVow.unwrap;
    const isGuest = this.props.isGuest;
    this.props.onGameCancelled(gameId, isGuest);
  }

  async resignGame() {
    const gameId = this.game().gameId;
    let handler = this.props.gameHandler;
    let resignVow = serviceStateSettingInterceptor(this, resignDisplay, handler, handler.resign, gameId);
    await resignVow.unwrap;
    const isGuest = this.props.isGuest;
    this.props.onGameResigned(gameId, isGuest);
  }

  getNewGameParams() {
    return this.initializeUserGameSettings().then(() => {
      let playParams = this.state.userGameSettings.playSettings;
      let startingPlayer = fixStartingPlayer(playParams.startingPlayer);
      playParams = {...playParams, startingPlayer};
      let dimension = playParams.dimension;
      let pointValues = PointValue.mkValueFactory(dimension).mkValueGrid();
      let gameParams = mkGameParams(playParams, pointValues);
      return gameParams;
    });
  }

  startGame() {
    this.getNewGameParams().then(newGameParams => {
      let handler = this.props.gameHandler;
      // TODO. If the very first play is a swap by the bot, the user is not informed. Do that.
      // Later bot swaps are properly messaged.
      serviceStateSettingInterceptor(this, startGameDisplay, handler, handler.start, newGameParams).passValue(
        (game) => {
          console.log(`game obtained from handler.start: ${stringify(game)}`);
          this.setStateGame(game);
          // this.setStateInfoMessage(startSuccessMessage(newGameParams));
        })
    });
  }

  renderWord(index, key) {
    let words = this.wordsPlayed();
    let l = words.length;
    let word = words[index].word;
    let wordRep = word.length > 0 ? word : '-----';
    let color = index === l - 1 ? 'FireBrick' : 'Chocolate';
    let backgroundColor = (index % 2) === 0 ? 'LemonChiffon' : 'Gold';

    return (
      <div key={key} style={{color: color, backgroundColor: backgroundColor, padding: '3px'}}>
        {wordRep}
      </div>
    )
  }

  scrollToLastWord() {
    if (this.wordsComponent) {
      this.wordsComponent.scrollTo(this.wordsPlayed().length - 1);
    }
  }

  /**
   * @param move - The piece and the destination point on the board: { piece, point }.
   */
  onMove(move) {
    const prevGame = this.game();
    if (noGame(prevGame)) { logPostMortemGame(); return; }
    // TODO. Could this call fail? If so need to set status.
    const game = prevGame.applyUserMove(move);
    this.setStateGame(game);
  }

  onRevertMove(piece) {
    const prevGame = this.game();
    if (noGame(prevGame)) { logPostMortemGame(); return; }
    const game = prevGame.revertMove(piece);
    this.setStateGame(game);
  }

  revertPlay() {
    let game = this.game();
    let updatedGame = game.revertPlay();
    this.setStateGame(updatedGame);
    this.setStateInfoMessage(null);
  }

  onSwap(piece) {
    const prevGame = this.game();
    let handler = this.props.gameHandler;
    serviceStateSettingInterceptor(this, swapDisplay, handler, handler.swapAndGetMachinePlay, prevGame, piece).passValue(
      (game) => this.setStateGame(game)
    )
  }

  PlayMenu = (props) => {
    let game = this.game();
    let running = game.running();
    let finished = game.finished();
    let playStarted = game.wordPlayStarted();
    let canCommit = running && playStarted;
    let canEnd = running || finished;
    let canRevert = running && playStarted;
    let canSuspend = running && !this.props.isGuest;
    let it = this;

    return (
      <div>
          <ButtonToolbar aria-label="header">
            <PlayButton disabled={!canCommit} onClick={it.commitPlay}>Commit Word</PlayButton>{space}
            <PlayButton disabled={!canRevert} onClick={it.revertPlay}>Undo</PlayButton>{space}
            <Dropdown>
              <PlayDropdownToggle id="dropdown-end" disabled={!canEnd} style={{height: '100%'}}>End</PlayDropdownToggle>
              <Dropdown.Menu>
                <PlayMenuItem disabled={!canSuspend} onClick={it.suspendGame}>Suspend</PlayMenuItem>
                <PlayMenuItem disabled={!running} onClick={it.resignGame}>Resign</PlayMenuItem>
                <PlayMenuItem disabled={!running} onClick={it.cancelGame}>Forget</PlayMenuItem>
                <PlayMenuItem disabled={!finished} onClick={() => it.props.onExit()}>Exit</PlayMenuItem>
              </Dropdown.Menu>
            </Dropdown>{space}
            <span style={{width: '20px'}}/>
            <PlayButton style={{float: "right"}}>
              <a href={rulesPublicUrl} target="_blank" rel="noopener noreferrer" style={{color: 'white'}}>Rules</a>
            </PlayButton>
          </ButtonToolbar>
      </div>
    )
  };

  renderScore(player, score) {
    return (
      <div>
        <label style={styles.playLabelStyle}>{player}: </label>
        <label style={styles.playScoreStyle}>{score}</label>
      </div>
    );
  }

  Tray = (props) => {
    let game = this.game();

    return <TrayComponent
      pieces={game.tray.pieces}
      canMovePiece={this.gameMethod().canMovePiece}
      squareSize={this.state.userGameSettings.lookAndFeelSettings.squareSize}
      enabled={game.running()}
      onRevertMove={this.onRevertMove}
    />
  };

  Board = (props) => {
    let game = this.game();
    let pointsInUserPlay = game.board.getUserMovePlayPieces().map(pp => pp.point);
    let machineMovePoints = game.machineMoves.map(piecePoint => piecePoint.point);
    console.log(`Board - completed initializations`);

    return <BoardComponent
      board={game.board}
      pointsInUserPlay={pointsInUserPlay}
      pointsMovedInMachinePlay={machineMovePoints}
      isLegalMove={this.gameMethod().legalMove}
      canMovePiece={this.gameMethod().canMovePiece}
      squareSize={this.state.userGameSettings.lookAndFeelSettings.squareSize}
      pointValues={game.pointValues}
      enabled={game.running()}
      onMove={this.onMove}
    />;
  };

  setWordsComponent(list) {
    this.wordsComponent = list;
  }

  RawPlayBody = (props) => {
    /*
     * Avoid errors if game is not yet set - early in the mount process.
     * State will be set in componentDidiMount leading to further renders.
     * So this empty body should never be shown.
     */
    if (this.game() === null)
      return (<div/>);

    let game = this.game();
    let userName = this.props.nickname.substring(0, 10);
    let userScore = game.score[Game.USER_INDEX];
    let machineScore = game.score[Game.MACHINE_INDEX];
    let isTrayPiece = this.gameMethod().isTrayPiece;
    let gamePlayCanSwap = game.running() && !game.wordPlayStarted();
    let onSwap = this.onSwap;
    let numWords = this.wordsPlayed().length;
    let it = this;

    console.log(`RawPlayBody - completed initializations`);

    return (
      <div style={{display: 'flex', flexDirection: 'column'}}>
        <it.PlayMenu />

        <div style={{display: 'flex', flexDirection: 'row', border: '1px solid GoldenRod',
          padding: '10px', margin: "15px auto 15px 0"}}>
          <it.Board/>{space}{space}
          <it.Tray />{space}{space}
          <div style={{display: 'flex', flexDirection: 'column', left: "3px"}}>
            <SwapBinComponent isTrayPiece={isTrayPiece} enabled={gamePlayCanSwap} onSwap={onSwap} />
            <div style={styles.playScoreBoxStyle}>
              {it.renderScore(userName, userScore)}
              {it.renderScore("Bot", machineScore)}
            </div>
            <div style={{height: 210}}>
            <div style={styles.playWordListStyle}>
              <ReactList
                ref={list => it.setWordsComponent(list)} length={numWords} type='uniform'
                itemRenderer={ (index, key) => it.renderWord(index, key) }
              />
            </div>
          </div>
          </div>
        </div>
        <it.StatusAsButton/>
        <div style={{paddingTop: '2px'}}>
          <label style={styles.playLightMessageStyle(true)}>{copyright}</label>
        </div>
      </div>
    )
  };

  render() {
    console.log(`play component render - state: ${stringify(this.state)}`);
    let errorCallback = () => this.props.onUnrecoverableError();
    let loginExpiredCallback = () => this.props.loginExpired();
    let dndBackend = this.state.dndBackend;
    let it = this;

    return (
      <DndProvider backend={dndBackend}>
        <ServiceProcessingDecorator
          comp={it}
          errorCallback={() => errorCallback()}
          loginExpiredCallback={() => loginExpiredCallback()}
        >
          <it.RawPlayBody />
        </ServiceProcessingDecorator>
      </DndProvider>
    )
  }
}

export function mapStateToProps(store) {
  return createStructuredSelector({
    gameHandler: selectGameHandler(),
    serverInfo: selectServerInfo(),
    loginEvidence: selectLoginEvidence,
    isGuest: selectIsGuest,
    nickname: selectNickname,
    gameIdToResume: selectGameIdToResume,
  })(store);
}

export function mapDispatchToProps(dispatch) {
  return {
    onUnrecoverableError: () => {
      dispatch(loggedOut());
      dispatch(push(authRoutingPaths.entry))
    },
    onGameSuspended: (gameId) => {
      dispatch(push(gameRoutingPaths.games));
    },
    onExit: () => {
      dispatch(push(gameRoutingPaths.games));
    },
    onGameCancelled: (gameId, isGuest) => {
      let redirect = isGuest ? authRoutingPaths.entry : gameRoutingPaths.games;
      dispatch(push(redirect));
    },
    onGameResigned: (gameId, isGuest) => {
      let redirect = isGuest ? authRoutingPaths.entry : gameRoutingPaths.games;
      dispatch(push(redirect));
    },
    resetResumedGameId: () => dispatch(gameSelectedForResumption(null)),
    loginExpired: () => {
      dispatch(loggedOut());
      dispatch(push(authRoutingPaths.entry));
    },
  }
}

const componentWithRouter = withRouter(PlayComponent);
export default connect(mapStateToProps, mapDispatchToProps)(componentWithRouter);
