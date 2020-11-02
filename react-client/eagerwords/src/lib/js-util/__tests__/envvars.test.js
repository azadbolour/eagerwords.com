/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

import {getServerPort} from '../util/envvars';

test('get server port', () => {
  console.log(`${getServerPort()}`)
});
