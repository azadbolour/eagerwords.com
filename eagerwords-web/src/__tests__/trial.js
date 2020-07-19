/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

let combine = function(f1, f2) {
  return function(i) {
    return f2(f1(i));
  }
};

let tester = {
  f1 : function(i) {
    return i;
  },

  f2: function(i) {
    return 2 * i;
  },

  c: function() {
    return combine(this.f1, this.f2)
  },

  d: function(i) {
    return combine(this.f1, this.f2)(i)
  }
};

console.log(combine(tester.f1, tester.f2)(10));
console.log((tester.c())(20));
console.log((tester.d)(20));
