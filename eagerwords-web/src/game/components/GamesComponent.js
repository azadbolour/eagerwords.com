/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React, { Component } from 'react';
import { connect } from 'react-redux';
import { push } from 'connected-react-router'
import {createStructuredSelector} from 'reselect'
import { Header } from 'semantic-ui-react';
import {loggedOut} from "../../auth/redux/AuthActions";
import {getUrlOfPublicResource} from "../../base/util/BrowserUtil";
import {gameSelectedForResumption} from "../redux/GameActions";
import {stringify} from "../../base/util/Logger";
import BootstrapTable from 'react-bootstrap-table-next';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-table2-paginator/dist/react-bootstrap-table2-paginator.min.css';
import paginationFactory from 'react-bootstrap-table2-paginator';
import PropTypes from "prop-types";
import ButtonToolbar from "react-bootstrap/ButtonToolbar";
import Button from "react-bootstrap/Button";
import {selectGameHandler} from "../redux/gameSelector";
import {authService} from "../../auth/service/AuthService";
import {selectLoginEvidence, selectNickname} from "../../auth/redux/AuthSelector";
import {defaultOpContext, opMockEffects,} from "../../base/components/ComponentProcessingStateManager";
import {
  checkLoggedIn,
  serviceStateSettingInterceptor,
  ServiceProcessingDecorator,
} from "../../base/components/ComponentProcessingStateManager";
import {GameHeader} from "./GameHeaderComponent";
import {GenericFooter} from "../../base/components/GenericFooter";
import {EulaTextComponent} from "./informational/EulaTextComponent";
import {PrivacyComponent} from "./informational/PrivacyComponent";
import {NoticesComponent} from "./informational/NoticesComponent";
import {AboutComponent} from "./informational/AboutComponent";
import {gameRoutingPaths} from "./GameRoutingPaths";
import {authRoutingPaths} from "../../auth/components/AuthRoutingPaths";
import {getUserGamesDisplay, resumeDisplay, unregisterUserDisplay} from "../service/GameHandler";
import {rulesHtmlName} from "./GameComponentConstants";
import Modal from "react-bootstrap/Modal";
import {getServerUrlBase} from "../../envvars";

const space = <pre> </pre>;

const addLeadingZeros = function(number, size) {
  let padding = '0'.repeat(size);
  let padded = `${padding}${number}`;
  return padded.substring(padded.length - size);
};

// TODO. Use correct locale later. Needs user's locale.
const dateFormatter = (epochSeconds) => {
  let pad = function(datePart) {
    return addLeadingZeros(datePart, 2);
  };
  let d = new Date();
  d.setTime(1000 * epochSeconds);
  let year = `${d.getFullYear()}`.substring(2);
  return `${d.getMonth() + 1}/${d.getDate()}/${year} ${pad(d.getHours())}:${pad(d.getMinutes())}`
};

/**
 Data returned in games is a list of:

    BasicGameInfo:
      gameId: ID,
      firstSecond: Long,
      lastSecond: Long,
      status: String,
      userScore: Int,
      machineScore: Int

    needs to be converted to, for example:
      gameId: 'a442a998', // 8-character prefix
      startDate: '1/21/20 15:30',
      score: 25 - 50,
      status: Suspended,
      status: ENDED
 */
const gameInfoToTableRow = (info) => {
  return {
    gameId: info.gameId,
    gameIdDisplay: `${info.gameId.substring(0, 8)}...`,
    startDate: dateFormatter(info.firstSecond),
    score: `${info.userScore} - ${info.machineScore}`,
    status: info.status,
  }
};

const gameTableColumns = (resumeFormatter, nickname) => [
  {
    dataField: 'gameId',
    text: 'Id',
    headerStyle: {
      backgroundColor: '#c8e6c9',
      width: '50px'
    },
    hidden: true
  },
  {
    dataField: 'gameIdDisplay',
    text: 'Id',
    headerStyle: {
      backgroundColor: '#c8e6c9',
      width: '50px'
    }
  },
  {
    dataField: 'startDate',
    text: 'Started',
    headerStyle: {
      backgroundColor: '#c8e6c9',
      width: '60px'
    }
  },
  {
    dataField: 'score',
    text: `${nickname} - Bot`,
    headerStyle: {
      backgroundColor: '#c8e6c9',
      width: '60px'
    }
  },
  {
    dataField: 'status',
    text: 'Status',
    headerStyle: {
      backgroundColor: '#c8e6c9',
      width: '50px'
    }
  },
  {
    dataField: 'resumeButton',
    isDummyField: true,
    text: '',
    headerStyle: (column, colIndex) => {
      return {
        backgroundColor: '#c8e6c9',
        width: '60px'
      }
    },
    formatter: resumeFormatter
  },
];

const helpPublicUrl = getUrlOfPublicResource(rulesHtmlName);

/**
 * The games components is protected.
 */
class GamesComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      opContext: defaultOpContext,
      games: [],
      showUnregister: false,
    };
    this.authService = authService;
  }

  static propTypes = {
    gameHandler: PropTypes.object.isRequired
  };

  UnregisterModal = (props) => {
    let show = this.state.showUnregister;
    let it = this;

    let unregistrer = () => {
      hide();
      let handler = this.props.gameHandler;
      serviceStateSettingInterceptor(it, unregisterUserDisplay, handler, handler.unregisterUser,
        opMockEffects(it)).then(result => it.props.onLogout());
    };

    let hide = () => it.setState((state) => {
      return {...state, showUnregister: false}
    });

    return (
      <Modal show={show}>
        <Modal.Header>
          <Modal.Title>Unregister from EagerWords?</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <div style={{fontSize: '20px'}}>
          Warning!
          Unregister removes all your data from EagerWords!
          You may sign up again with the same email.
          But the removal of your game and other information is permanent!
          </div>
        </Modal.Body>
        <Modal.Footer>
        <ButtonToolbar>
          <Button variant="secondary" size="sm" onClick={() => hide()}>
            Cancel
          </Button>{space}
          <Button variant="warning" size="sm" onClick={() => unregistrer()}>
            OK. Unregister and remove all my data!
          </Button>{space}
        </ButtonToolbar>
        </Modal.Footer>
      </Modal>
    );

  };

  // cell is null?? row is the row data structure whose schema is defined in columns.
  boundRenderResumeButton = (cell, row) => {
    return this.renderResumeButton(this, cell, row);
  };

  /*
   * Note. For performance reasons, maxGetGames configured in the server (default
   * 500) is the absolute limit on the number of games retrieved in one query.
   * If more are needed, use time limits to get successive chunks (not
   * yet implemented in the server as of March 2020).
   */
  maxGames = 300;

  /*
   * The first implementation of getUserGames in the server ignores time limits
   * for the games to obtain, and just get the latest n games where n is
   * the minimum of maxGames defined here, and maxGetGames configured for the server.
   *
   * For now we just use that implementation and ignore older games.
   *
   * For proper paging, use time limits and maxGames to limit the amount of data
   * obtained from the server at each scroll of pages.
   */

  componentDidMount = async () => {
    let it = this;
    // console.log(`GamesComponent - in componentDidMount`);
    let loggedIn = await checkLoggedIn(this);
    // console.log(`messages: ${stringify(this.state.opContext.messages)}`);
    // console.log(`loggedIn: ${stringify(loggedIn)}`);
    /*
     * The function checkLoggedIn will set the loggedOutMsg of this component.
     * Then the service processing decorator that wraps the contents of the
     * component will check the loggedOutMsg, remove any login evidence and send
     * control back to the entry component. So no further action is needed here.
     */
    if (!loggedIn) {
      return;
    }
    let handler = this.props.gameHandler;
    serviceStateSettingInterceptor(this, getUserGamesDisplay, handler, handler.getUserGames,
      0, 0, this.maxGames, opMockEffects(this))
      .passValue(gamesData => {
        if (!gamesData) gamesData = [];
        let games = gamesData.map(info => gameInfoToTableRow(info));
        it.setState((state) => {return {...state, games}})
      }
    );
  };

  /**
   * Resume a suspended game.
   *
   * Note that any game can be resurrected, i.e., shown in the PlayComponent.
   * But suspended games need to be activated first.
   */
  resumeGame = async function(row) {
    let status = row.status;
    let gameId = row.gameId;
    if (status === 'SUSPENDED') {
      let handler = this.props.gameHandler;
      let resumedVow = serviceStateSettingInterceptor(this, resumeDisplay, handler, handler.resume, gameId,
        opMockEffects(this));
      await resumedVow.unwrap;
    }
    this.props.onResume(gameId);
  };

  renderMenu = function() {
    let onPlay = this.props.onPlay;
    let onGoToSettings = this.props.onGoToSettings;

    return (
      <div style={{display: 'flex', flexDirection: 'row'}}>
        <ButtonToolbar aria-label="main">
          <Button size="sm" variant="success" onClick={() => onPlay({})}>New Game</Button>
          &nbsp;&nbsp;
          <Button size="sm" variant="success" onClick={onGoToSettings}>Settings</Button>
          &nbsp;&nbsp;
        </ButtonToolbar>
      </div>
    )
  };

  renderResumeButton(me, cell, row) {
    let status = row.status;
    let label = 'resume';
    if (status === 'ENDED' || status === 'RESIGNED')
      label = 'show';
    return (
      <Button size="sm" variant="success" onClick={() => {me.resumeGame(row)}}>{label}</Button>
    );
  }

  renderBody() {
    let games = this.state.games;
    let resumeFormatter = this.boundRenderResumeButton;
    let nickname = this.props.nickname;
    return (
      <div>
        {this.renderMenu()}
        <div style={{fontSize: 12, border: '1px solid GoldenRod', padding: '5px', display: 'flex', flexDirection: 'column', marginTop: "8px"}}>
          <Header as="h5" align='center' style={{color: 'Green'}}>Games</Header>
          <BootstrapTable
            keyField='gameId'
            data={[...games]}
            columns={gameTableColumns(resumeFormatter, nickname)}
            bordered={true}
            striped
            condensed
            hover
            pagination={ paginationFactory({
              sizePerPage: 4
            })}
          />
        </div>
      </div>
    )
  }

  render() {
    let errorCallback = () => this.props.onUnrecoverableError();
    let loginExpiredCallback = () => this.props.loginExpired();

    let userName = this.props.nickname;
    let loginEvidence = this.props.loginEvidence;
    let it = this;

    let logoutCallback = () => {
      // TODO. Call logout service method.
      // For now just removing the login evidence in the UI is sufficient.
      this.props.onLogout();
    };

    let unregisterCallback = () => {
      this.setState((state, props) => {
        return {...state, showUnregister: true}
      });
    };

    let Footer = () => {return (<GenericFooter
      EulaTextComponent={EulaTextComponent}
      PrivacyComponent={PrivacyComponent}
      NoticesComponent={NoticesComponent}
      AboutComponent={AboutComponent}
    />)};

    return (
      <div>
        <GameHeader loginEvidence={loginEvidence} onLogout={logoutCallback} onUnregisterUser={unregisterCallback} helpUrl={helpPublicUrl}/>
        <Header as="h3" textAlign="center" style={{color: 'DarkGoldenRod'}}>Eager Words - Welcome {userName}!</Header>
        <div>{space}</div>

        <ServiceProcessingDecorator
          comp={it}
          errorCallback={() => errorCallback()}
          loginExpiredCallback={() => loginExpiredCallback()}
        >
          <it.UnregisterModal/>
          <div>
            {this.renderBody()}
          </div>
          <Footer/>
        </ServiceProcessingDecorator>
      </div>
    );
  }
}

export function mapDispatchToProps(dispatch) {
  return {
    onUnrecoverableError: () => {
      dispatch(loggedOut());
      dispatch(push(authRoutingPaths.entry))
    },
    onPlay: (gameId) => {
      dispatch(push(gameRoutingPaths.play));
    },
    onResume: (gameId) => {
      dispatch(gameSelectedForResumption(gameId));
      dispatch(push(gameRoutingPaths.play));
    },
    onGoToSettings: () => dispatch(push(gameRoutingPaths.settings)),
    onLogout: () => {
      dispatch(loggedOut());
      dispatch(push(authRoutingPaths.entry));
    },
    loginExpired: () => {
      dispatch(loggedOut());
      dispatch(push(authRoutingPaths.entry));
    },
  }
}

export function mapStateToProps(store) {
  return createStructuredSelector({
    gameHandler: selectGameHandler(),
    nickname: selectNickname,
    loginEvidence: selectLoginEvidence
  })(store);
}

export default connect(mapStateToProps, mapDispatchToProps)(GamesComponent);
