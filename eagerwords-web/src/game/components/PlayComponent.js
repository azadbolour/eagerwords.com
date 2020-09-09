/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import PropTypes from 'prop-types';
import {default as TouchBackend} from 'react-dnd-touch-backend';
import React from 'react';
import ReactList from 'react-list';
import * as Game from '../domain/Game';
import {logPostMortemGame, noGame} from '../domain/Game';
import TrayComponent from './TrayComponent';
import BoardComponent from './BoardComponent';
import SwapBinComponent from './SwapBinComponent';
import {OK} from '../../base/service/ServiceHelper';
import * as PointValue from "../domain/PointValue";
// import {stringify} from "../util/Logger";
import * as styles from '../css/PlayStyles';
import * as BrowserUtil from "../../base/util/BrowserUtil";
import {deviceTypes} from "../../base/domain/DeviceTypes";
import {defaultGameSettings, fixStartingPlayer,} from '../domain/GameSettings';
import {DndProvider} from 'react-dnd';
import HTML5Backend from 'react-dnd-html5-backend';
import {connect} from 'react-redux';
import {createStructuredSelector} from 'reselect';
import {selectIsGuest, selectLoginEvidence, selectNickname} from "../../auth/redux/AuthSelector"
import {gameSelectedForResumption,} from "../redux/GameActions";
import {selectServerInfo} from "../../base/redux/baseSelector";
import {selectGameHandler, selectGameIdToResume,} from "../redux/gameSelector";
import {push} from "connected-react-router";
import {withRouter} from "react-router";
import ButtonToolbar from "react-bootstrap/ButtonToolbar";
import Dropdown from "react-bootstrap/Dropdown";
import {gameRoutingPaths} from "./GameRoutingPaths";
import {authRoutingPaths} from "../../auth/components/AuthRoutingPaths";

import {
  checkLoggedIn,
  defaultOpContext,
  ServiceProcessingDecorator,
  serviceStateSettingInterceptor,
  setOpAlertMsg,
} from "../../base/components/ComponentProcessingStateManager";

import {authService} from "../../auth/service/AuthService";
import {stringify} from "../../base/util/Logger";
import {machineStarts} from "../domain/GameParams";
import {valueVow} from "../../base/domain/Vow";

import {
  cancelDisplay,
  commitPlayDisplay,
  getFullGameDisplay,
  getUserGameSettingsDisplay,
  resignDisplay,
  startGameDisplay,
  suspendDisplay,
  swapDisplay
} from "../service/GameHandler";
import {PlayButton, PlayDropdownToggle, PlayMenuItem} from "../css/PlayStyles";
import {loggedOut} from "../../auth/redux/AuthActions";
import {getUrlOfPublicResource} from "../../base/util/BrowserUtil";
import {rulesHtmlName} from "./GameComponentConstants";

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
  if (plays.machinePlayedLast && plays.lastPlayIsSwap)
    return 'bot took a pass';
  return "OK";
};

// const Main = ({hidden, children}) => <div style={{
//   display: hidden ? 'none' : 'block'
// }}>
//   {children}
// </div>;

const USER_START_MESSAGE = "your play ...";
const MACHINE_START_MESSAGE = "OK";

const startSuccessMessage = gameParams =>
  machineStarts(gameParams) ? MACHINE_START_MESSAGE : USER_START_MESSAGE;

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
      gameSettings: null,
      dndBackend: HTML5Backend
    };

    this.wordsComponent = null;
  }

  static propTypes = {
    gameHandler: PropTypes.object.isRequired,
    serverInfo: PropTypes.object.isRequired,
  };

  async componentDidMount() {
    if (this.props.isGuest) {
      this.startGame();
      return;
    }

    let loggedIn = await checkLoggedIn(this);
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

  async initializeGameSettings() {
    let handler = this.props.gameHandler;
    let vow = this.props.isGuest ?
      valueVow(defaultGameSettings) :
      serviceStateSettingInterceptor(this, getUserGameSettingsDisplay, handler, handler.getUserGameSettings);
    let result = await vow.unwrap;
    let settings = result.ok ? result.data : null;
    if (settings === null)
      settings = defaultGameSettings;

    const preferredDevice = settings.preferredDevice;
    let {device} = BrowserUtil.inputDeviceInfo(preferredDevice);
    let dndBackend = device === deviceTypes.touch ? TouchBackend : HTML5Backend;
    this.setState((state) => {return {...state, dndBackend, gameSettings: settings}});
  }

  resumeGame(gameId) {
    let handler = this.props.gameHandler;
    serviceStateSettingInterceptor(this, getFullGameDisplay, handler, handler.getFullGame, gameId).passValue(
      (game) => {
        this.setStateGame(game);
        this.scrollToLastWord();
      }
    )
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
    let gameSettings = game ? game.gameParams : null;
    // TODO. URGENT. Add dnd backend to the state for this case.
    this.setState((state) => {return {...state, game, gameSettings}});
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

  status() {
    // return this.state.gameState.message;
    const errorStatus = this.state.opContext.messages.alertMsg;
    return errorStatus ? errorStatus : infoStatus(this.game());
  }

  commitPlay() {
    let handler = this.props.gameHandler;
    serviceStateSettingInterceptor(this, commitPlayDisplay, handler, handler.commitPlayAndGetMachinePlay, this.game()).passValue(
      ((game) => this.setStateGame(game))
    )
  }

  async suspendGame() {
    const gameId = this.game().gameId;
    let handler = this.props.gameHandler;
    let suspendVow = serviceStateSettingInterceptor(this, suspendDisplay, handler, handler.suspend, gameId);
    await suspendVow.unwrap;
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
    return this.initializeGameSettings().then(() => {
      console.log(`state gameSettings: ${stringify(this.state.gameSettings)}`);
      let settings = fixStartingPlayer(this.state.gameSettings);
      let dimension = settings.dimension;
      let pointValues = PointValue.mkValueFactory(dimension).mkValueGrid();
      return {...settings, pointValues};
    });
  }

  startGame() {
    this.getNewGameParams().then(newGameParams => {
      let handler = this.props.gameHandler;
      // TODO. If the very first play is a swap by the bot, the user is not informed. Do that.
      // Later bot swaps are properly messaged.
      serviceStateSettingInterceptor(this, startGameDisplay, handler, handler.start, newGameParams).passValue(
        (game) => {
          this.setStateGame(game);
          this.setStateInfoMessage(startSuccessMessage(newGameParams));
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
  }

  onSwap(piece) {
    const prevGame = this.game();
    let handler = this.props.gameHandler;
    serviceStateSettingInterceptor(this, swapDisplay, handler, handler.swapAndGetMachinePlay, prevGame, piece).passValue(
      (game) => this.setStateGame(game)
    )
  }

  // gotoEntry = () => {
  //   this.props.showEntry();
  // };

  // TODO. Add Quit - enabled if a completed game is being shown.

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
        <div style={{display: 'flex', flexDirection: 'row', backgroundColor: 'LightGrey', padding: '3px'}}>
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
            <span style={{width: '390px'}}/>
            <PlayButton style={{float: "right"}}>
              <a href={rulesPublicUrl} target="_blank" style={{color: 'white'}}>Rules</a>
            </PlayButton>
          </ButtonToolbar>
        </div>
        <div>
          <span style={{height: '5px'}}>{space}</span>
        </div>
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
      squarePixels={this.state.gameSettings.squarePixels}
      enabled={game.running()}
      onRevertMove={this.onRevertMove}
    />
  };

  Board = (props) => {
    let game = this.game();
    let pointsInUserPlay = game.board.getUserMovePlayPieces().map(pp => pp.point);
    let machineMovePoints = game.machineMoves.map(piecePoint => piecePoint.point);

    return <BoardComponent
      board={game.board}
      pointsInUserPlay={pointsInUserPlay}
      pointsMovedInMachinePlay={machineMovePoints}
      isLegalMove={this.gameMethod().legalMove}
      canMovePiece={this.gameMethod().canMovePiece}
      squarePixels={this.state.gameSettings.squarePixels}
      pointValues={game.pointValues}
      enabled={game.running()}
      onMove={this.onMove}
    />;
  };

  setWordsComponent(list) {
    this.wordsComponent = list;
  }

  render() {
    if (this.game() === null) // Very first render - state not yet set.
      return (<div/>);

    let errorCallback = () => this.props.onUnrecoverableError();
    let loginExpiredCallback = () => this.props.loginExpired();
    let game = this.game();
    let status = this.status();
    let userName = this.props.nickname.substring(0, 10);
    let userScore = game.score[Game.USER_INDEX];
    let machineScore = game.score[Game.MACHINE_INDEX];
    let isTrayPiece = this.gameMethod().isTrayPiece;
    let gamePlayCanSwap = game.running() && !game.wordPlayStarted();
    let onSwap = this.onSwap;
    let dndBackend = this.state.dndBackend;
    let numWords = this.wordsPlayed().length;
    let it = this;

    return (
      <DndProvider backend={dndBackend}>
        <ServiceProcessingDecorator
          comp={it}
          errorCallback={() => errorCallback()}
          loginExpiredCallback={() => loginExpiredCallback()}
        >
            <div style={{display: 'flex', flexDirection: 'column'}}>
              <it.PlayMenu />
              <div style={{display: 'flex', flexDirection: 'row', border: '1px solid GoldenRod',
                  padding: '15px', margin: "8px auto 8px 0"}}>
                <it.Board/>{space}{space}
                <it.Tray />{space}{space}
                <div style={{display: 'flex', flexDirection: 'column', left: "3px"}}>
                  <SwapBinComponent isTrayPiece={isTrayPiece} enabled={gamePlayCanSwap} onSwap={onSwap} />
                  <div style={styles.playScoreBoxStyle}>
                    {it.renderScore(userName, userScore)}
                    {it.renderScore("Bot", machineScore)}
                  </div>
                  <div style={styles.playWordListStyle}>
                    <ReactList
                      ref={list => it.setWordsComponent(list)} length={numWords} type='uniform'
                      itemRenderer={ (index, key) => it.renderWord(index, key) }
                    />
                  </div>
                </div>
              </div>
              <div style={styles.playStatusStyle}>{status}</div>
              <div style={{paddingTop: '2px'}}>
                <label style={styles.playLightMessageStyle(true)}>{copyright}</label>
              </div>
            </div>
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
