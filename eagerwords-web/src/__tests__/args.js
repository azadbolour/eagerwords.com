/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords/blob/master/LICENSE.md
 */

let f = (x, y = 5) => x + y;

console.log(`${f(10)}`);
console.log(`${f(10, 2)}`);

let obj2 = {
  times: (x) => 2 * x,
  plus: (x) => 2 + x
};

let obj3 = {
  times: (x) => 3 * x,
  plus: (x) => 3 + x
};

let docall = (obj, fname) => obj[fname](100);

console.log(`${docall(obj2, 'times')}`);
console.log(`${docall(obj3, 'plus')}`);

// For objects you have to use proper functions not arrow functions.
// For classes - methods are proper functions.

let obj5 = {
  f: function(a, b, c) {return this.g(a, b, c);},
  g: function(a, b, c) {return a + b + c; }
};

let func1 = (receiver, func, ...args) => func.apply(receiver, args)
let func2 = (receiver, func, ...args) => func1(receiver, func, ...args)

console.log(`${func2(obj5, obj5.f, 1, 10, 100)}`);
console.log(`${obj5.f(1, 10, 100)}`);

class Tester {
  constructor() {}

  f(a, b, c) {return this.g(a, b, c);}

  g(a, b, c) {return a + b + c; }
};

let tester = new Tester();

console.log(`${func2(tester, tester.f, 1, 10, 100)}`);
console.log(`${tester.f(1, 10, 100)}`);

