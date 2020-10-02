/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

// TODO. URGENT. Move to base.

import {getApiType} from '../../envvars';

test('get api env', () => {
  console.log(`${getApiType()}`)
});
