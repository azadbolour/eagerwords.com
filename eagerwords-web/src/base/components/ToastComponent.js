/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React from 'react';
import {ToastContainer, cssTransition as toastCssTransition} from 'react-toastify';

const ToastTransition = toastCssTransition({
  enter: 'Toastify__slide-enter',
  exit: 'Toastify__slide-exit',
  duration: [300, 100],
  appendPosition: true
})

export default function ToastComponent() {
  return <ToastContainer
    transition={ToastTransition}
    hideProgressBar={true}
    autoClose={8000}
  />
}
