/* global exports */
"use strict"

exports.dummyD3Fn   = function (selection) { console.log("dummyD3Fn: " + selection); }

exports.d3SelectFn   = function (selector) {
    return d3.select(selector);
}

exports.d3SelectAllFn = function (selector, selection) {
    return selection(selector);
}

exports.d3DataFn = function (data, selection) {
    return selection(data);
}
// core D3 function dummies, to be replaced with actual D3 calls
var d3 = {
    select: function (selector) {
                console.log("d3Select: " + selector);
            }

}
