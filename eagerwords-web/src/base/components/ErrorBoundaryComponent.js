/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

import React, { Component } from 'react';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-table2-paginator/dist/react-bootstrap-table2-paginator.min.css';
import {error} from '../util/MiscUtil';

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

    return <>
      {this.props.children}
    </>
  }
}

export default ErrorBoundaryComponent;
