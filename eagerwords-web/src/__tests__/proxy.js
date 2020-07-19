/*
 * Copyright 2017-2020 Azad Bolour
 * Licensed under GNU Affero General Public License v3.0 -
 *   https://github.com/azadbolour/eagerwords.com/blob/master/LICENSE.md
 */

function traceMethodCalls(obj) {
    const handler = {
        get(target, propKey, receiver) {
            const targetValue = Reflect.get(target, propKey, receiver);
            if (typeof targetValue === 'function') {
                return function (...args) {
                    console.log('CALL', propKey, args);
                    return targetValue.apply(this, args); // (A)
                }
            } else {
                return targetValue;
            }
        }
    };
    return new Proxy(obj, handler);    
}

const obj1 = {
    f: function(a, b, c) { return this.g(a, b, c) ; },

    g: function(a, b, c) { return a + b + c ; }
};

const proxy1 = traceMethodCalls(obj1);

let sum = proxy1.f(1, 10, 100);

console.log(sum);
