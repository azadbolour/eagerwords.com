/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import detectIt from 'detect-it';
import {deviceTypes, validInputDevices} from "../domain/DeviceTypes";

export const hasDragAndDrop = function() {
  let div = window.document.createElement('div');
  return ('draggable' in div) || ('ondragstart' in div && 'ondrop' in div);
};

/*
 * Note. For MAC, detectIt only detects touch even when an external mouse is attached.
 * But that seems to be harmless. So we'll let that go for now. Ideally should find out
 * if a mouse is attached. But I don't know how. If issues arise, check if the
 * machine is a MAC or one of the main devices for which primary pointing is by touch,
 * and if the user actually prefers the mouse, assume that a mouse is attached.
 *
 * TODO. Implement this strategy. Will need to know the user's choice.
 * So inputDeviceInfo needs to get the user's actual choice not the default.
 * It can get the defaults here. We'd need to add preferredPointingDevice to
 * the game preferences, and allow it to be unspecified.
 */

/**
 * Get the name of the pointing device to use.
 * @param preferredDevice The user's preferred device ('mouse' or 'touch') -
 *    returned if available in current browser.
 * @returns 'mouse' or 'touch' or undefined.
 */
export const inputDeviceInfo = function(preferredDevice) {
  let primaryDevice = detectPrimaryDevice();

  const availableResponse = function(device) {
    return {device};
  };

  // No preference specified.
  if (preferredDevice === undefined || preferredDevice === null)
    return availableResponse(primaryDevice);

  // let {valid: preferredDeviceIsValid} = validatePreferredInputDevice(preferredDevice);

  // Preference is invalid
  // if (!preferredDeviceIsValid)
  //   return {
  //     device: primaryDevice,
  //     message: `preferred device '${preferredDevice}' is invalid`
  //   };

  // Preference is available.
  if (isDeviceAvailable(preferredDevice))
    return availableResponse(preferredDevice);

  // Preference is unavailable according to detectIt
  // (but not necessarily actually unavailable).
  return availableResponse(primaryDevice); // The best we can do for now - just report the primary device.
};

function isDeviceAvailable(deviceName) {
  if (deviceName === undefined)
    return false;

  switch (deviceName) {
    case deviceTypes.mouse:
      return detectIt.hasMouse || (detectIt.primaryInput === deviceTypes.mouse);
    case deviceTypes.touch:
      return detectIt.hasTouch || (detectIt.primaryInput === deviceTypes.touch);
    default:
      return false;
  }
}

/**
 * If no primary input is detectable then default to mouse if available,
 * otherwise to touch if available.
 */
function detectPrimaryDevice() {
  let primary = detectIt.primaryInput;
  if (validInputDevices.includes(primary))
    return primary;
  if (detectIt.hasMouse)
    return deviceTypes.mouse;
  if (detectIt.hasTouch)
    return deviceTypes.touch;
  return deviceTypes.mouse; // For good measure.
}
