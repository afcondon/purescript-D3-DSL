/* global exports */
"use strict"

exports.runD3Fn   = function (selection) { console.log("runD3Fn: " + selection); }

exports.d3SelectFn   = function (selector) {
    return d3.select(selector);
}

var d3 = {}
d3.select = function (selector) {
    console.log("d3Select: " + selector);
}
