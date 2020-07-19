/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

/**
 * Information about the user as obtained from the authentication
 * provider, but independent of the provider's naming conventions.
 * Initially just use the name for greeting the user and identifying
 * him on the screen. So just use first name.
 */
export const mkUser = function(userId, name, email, licenseAccepted) {
  let _userId = userId;
  let _name = name;
  let _email = email;
  let _licenseAccepted = licenseAccepted;

  return {
    get userId() { return _userId; },
    get name() {return _name; },
    get email() {return _email; },
    get licenseAccepted() {return _licenseAccepted; }
  }

};
