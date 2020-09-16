/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React, { Component } from 'react';
import { Header } from 'semantic-ui-react';
import { connect } from 'react-redux';

import { push } from 'connected-react-router'
import {createStructuredSelector} from 'reselect'
import {
  validDimensions,
  validTrayCapacity,
  pieceProviderTypes,
  playerTypes,
  randomPlayerType,
  defaultLanguageCode,
  defaultGamePlayParams,
} from '../domain/GamePlayParams';

import {
  validSquareSizes,
  squareSizeToPixels,
  defaultGameLookAndFeelParams,
} from '../domain/GameLookAndFeelParams';

import {defaultUserGameSettings, mkUserGameSettings} from "../domain/UserGameSettings";

import {deviceTypes} from "../../base/domain/DeviceTypes"
import Select from "../../base/components/Select";
import {selectGameHandler} from "../redux/gameSelector";
import {ifEmpty, strToNumber} from "../../base/util/MiscUtil";
import {stringify} from "../../base/util/Logger";
import ButtonToolbar from "react-bootstrap/ButtonToolbar";
import Button from "react-bootstrap/Button";
import {error, warn, nil, getOrElseF} from '../../base/util/MiscUtil';
import {authService} from "../../auth/service/AuthService";
import {selectLoginEvidence, selectNickname} from "../../auth/redux/AuthSelector";
import {gameRoutingPaths} from "./GameRoutingPaths";

import {defaultOpContext, opMockEffects} from "../../base/components/ComponentProcessingStateManager";
import {
  checkLoggedIn,
  serviceStateSettingInterceptor,
  ServiceProcessingDecorator,
} from "../../base/components/ComponentProcessingStateManager";
import {getUserGameSettingsDisplay, saveUserGameSettingsDisplay} from "../service/GameHandler";
import {loggedOut} from "../../auth/redux/AuthActions";
import {authRoutingPaths} from "../../auth/components/AuthRoutingPaths";


/**
 * Strip off properties that are not needed for the component itself.
 */
const inputProps = function(control){
  return {...control, touched: undefined, valid: undefined};
};

//
// const squareSizeForPixels = function(pixels) {
//   const fallback = 'small';
//   if (nil(pixels)) {
//     warn('squareSizeForPixels: pixels is nil');
//     return fallback;
//   }
//   let size = getOrElseF(squarePixelsToSize, pixels, () => {
//     warn("squarePixelsToSize: can't convert pixel size:", pixels);
//     return fallback;
//   });
//   return size;
// };

const controlsToSettings  = (controls) => {
  let playSettings = {
    dimension: controls.dimension.value,
    trayCapacity: controls.trayCapacity.value,
    languageCode: defaultLanguageCode,
    pieceProviderType: pieceProviderTypes.random,
    startingPlayer: controls.startingPlayer.value,
  };

  let lookAndFeelSettings = {
    squareSize: controls.squareSize.value,
    preferredDevice: controls.preferredDevice.value === 'None' ? null : controls.preferredDevice.value
  };

  return mkUserGameSettings(playSettings, lookAndFeelSettings);
};

const settingsToControls = (settings) => {
  let playSettings = settings.playSettings;
  let lookAndFeelSettings = settings.lookAndFeelSettings;
  return {
    dimension: {
      value: playSettings.dimension,
      placeholder: 'Dimension',
      valid: false,
      touched: false,
      options: validDimensions.map(function (dim) {
        return {value: dim, displayValue: dim}
      })
    },
    squareSize: {
      value: lookAndFeelSettings.squareSize,
      placeholder: 'Square Size',
      valid: false,
      touched: false,
      options: validSquareSizes.map(
        (size) => ({value: size, displayValue: size})
      )
    },
    trayCapacity: {
      value: playSettings.trayCapacity,
      placeholder: 'Tray Capacity',
      valid: false,
      touched: false,
      options: validTrayCapacity.map(
        (cap) => ({value: cap, displayValue: cap})
      )
    },
    startingPlayer: {
      value: playSettings.startingPlayer,
      placeholder: 'Starting Player',
      valid: false,
      touched: false,
      options: [
        {value: randomPlayerType, displayValue: randomPlayerType, isDefault: true},
        {value: playerTypes.userPlayer, displayValue: playerTypes.userPlayer},
        {value: playerTypes.machinePlayer, displayValue: playerTypes.machinePlayer},
      ]
    },
    preferredDevice: {
      value: lookAndFeelSettings.preferredDevice === null ? 'None' : settings.preferredDevice,
      placeholder: 'Preferred Device',
      valid: false,
      touched: false,
      options: [
        {value: 'None', displayValue: 'None', isDefault: true},
        {value: deviceTypes.mouse, displayValue: deviceTypes.mouse},
        {value: deviceTypes.touch, displayValue: deviceTypes.touch},
      ]
    }
  }
};

class SettingsComponent extends Component {
  constructor(props) {
    super(props);

    // TODO. Add validations.

    this.state = {
      opContext: defaultOpContext,
      settings: null,
      formControls: null,
    };
    this.authService = authService;
  }

  componentDidMount = async () => {
    let loggedIn = await checkLoggedIn(this);
    if (!loggedIn)
      return;

    let handler = this.props.gameHandler;
    let vow = serviceStateSettingInterceptor(this, getUserGameSettingsDisplay, handler, handler.getUserGameSettings);
    let result = await vow.unwrap;
    let settings = result.ok ? result.data : null;
    if (settings === null)
      settings = defaultUserGameSettings;
    let formControls = settingsToControls(settings);
    this.setState((state) => {return {...state, settings, formControls}});
  };

  async saveSettingsAndExit() {
    const settings = controlsToSettings(this.state.formControls);
    let handler = this.props.gameHandler;
    let vow = serviceStateSettingInterceptor(this, saveUserGameSettingsDisplay, handler, handler.saveUserGameSettings, settings,
      opMockEffects(this));
    await vow.unwrap;
    this.props.returnToGames()
  }

  doCancel() {
    this.props.dispatchCancel();
  }

  onChange = (event) => {
    const name = event.target.name;
    const strValue = event.target.value;

    // TODO. Clean up integer conversion code.
    const intFields = ['dimension', 'squarePixels', 'trayCapacity'];
    const value = intFields.includes(name) ? strToNumber(strValue) : strValue;

    const updatedControls = {
      ...this.state.formControls
    };
    const updatedFormElement = {
      ...updatedControls[name]
    };
    updatedFormElement.value = value;
    updatedFormElement.touched = true;

    updatedControls[name] = updatedFormElement;

    // console.log(`updated controls: ${stringify(updatedControls)}`);

    // TODO. URGENT. Use function for setState.
    this.setState({
      formControls: updatedControls
    });

  };

  renderForm() {
    const controls = this.state.formControls;
    if (controls === null)
      return (<div/>);

    const dimension = inputProps(controls.dimension);
    const squareSize = inputProps(controls.squareSize);
    const trayCapacity = inputProps(controls.trayCapacity);
    const startingPlayer = inputProps(controls.startingPlayer);
    const preferredDevice = inputProps(controls.preferredDevice);

    return (
      <div style={{border: '1px solid GoldenRod', padding: '10px', display: 'flex', flexDirection: 'column'}}>
        <Select name="dimension" {...dimension} onChange={this.onChange}/>
        <Select name='squareSize' {...squareSize} onChange={this.onChange}/>
        <Select name='trayCapacity' {...trayCapacity} onChange={this.onChange}/>
        <Select name='startingPlayer' {...startingPlayer} onChange={this.onChange}/>
        <Select name='preferredDevice' {...preferredDevice} onChange={this.onChange}/>
      </div>
    )
  }

  renderButtons() {
    return (
      <div style={{display: 'flex', flexDirection: 'row'}}>
        <ButtonToolbar aria-label="main">
          <Button size="sm" variant="success" onClick={() => this.saveSettingsAndExit()}>Save</Button>
          &nbsp;&nbsp;
          <Button size="sm" variant="success" onClick={() => this.doCancel()}>Cancel</Button>
        </ButtonToolbar>
      </div>
    )
  }

  renderBody() {
    return (
      <div>
        <div>
          {this.renderForm()}
        </div>
        <div>&nbsp;</div>
        <div>
          {this.renderButtons()}
        </div>
      </div>
    )
  }

  render() {
    let errorCallback = () => this.props.onUnrecoverableError();
    let loginExpiredCallback = () => this.props.loginExpired();
    let it = this;

    return (
      <div>
        <Header as="h1" textAlign="center" style={{color: 'Green'}}>Game Settings</Header>

        <ServiceProcessingDecorator
          comp={it}
          errorCallback={() => errorCallback()}
          loginExpiredCallback={() => loginExpiredCallback()}
        >
          <div style={{display: 'flex', flexDirection: 'row'}}>
            <div>
              <div>
                {this.renderBody()}
              </div>
            </div>
          </div>
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
    returnToGames: () => {
      dispatch(push(gameRoutingPaths.games))
    },
    dispatchCancel: () => dispatch(push(gameRoutingPaths.games)),
    loginExpired: () => {
      dispatch(loggedOut());
      dispatch(push(authRoutingPaths.entry));
    },
  };
}

export function mapStateToProps(store) {
  return createStructuredSelector({
    gameHandler: selectGameHandler(),
    // settings: selectUserGameSettings(),
    nickname: selectNickname,
    loginEvidence: selectLoginEvidence,
  })(store);
}

export default connect(mapStateToProps, mapDispatchToProps)(SettingsComponent);
