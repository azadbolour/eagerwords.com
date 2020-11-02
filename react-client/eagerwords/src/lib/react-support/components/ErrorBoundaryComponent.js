/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import React, { Component } from 'react';
import {toast} from 'react-toastify';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-table2-paginator/dist/react-bootstrap-table2-paginator.min.css';

const _problem = (consoleMethod, showToast) => (...msg) => {
  let theMsg = msg.join(' ');
  if (showToast) {
    toast.dismiss();
    toast('Oops! Something went wrong. Please try again later.');
  }
  console[consoleMethod](theMsg);
};

export const error = _problem('error', true);

class ErrorBoundaryComponent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      hasError: false
    };
  }

  componentDidCatch(err, errorInfo) {
    error('Error caught at boundary ErrorBoundaryComponent:', err, '\n\n Component stack:', errorInfo.componentStack);
  }

  static getDerivedStateFromError(err) {
    return {hasError: true};
  }

  render() {
    const {state} = this;
    const {hasError} = state;

    if (hasError) return <div>
      We're sorry, something has gone wrong. Please reload the page and try again.
    </div>

    return <div>
      {this.props.children}
    </div>
  }
}

export default ErrorBoundaryComponent;
