/* global exports */
"use strict"

exports.d3SelectFn   = function (selector) {
    return d3.select(selector);
}

exports.d3SelectAllFn = function (selector, selection) {
    return selection(selector);
}

exports.d3MergeFn = function (merged, original) {
    return orignal.merge(merged);
}

exports.d3AppendFn = function (element, selection) {
    return selection.append(element);
}

exports.d3RemoveFn = function(selection) {
    return selection.remove();
}

exports.d3EnterFn = function(selection) {
    return selection.enter();
}

exports.d3ExitFn = function (selection) {
    return selection.exit();
}

exports.d3TransitionFn = function(t, selection) {
    return selection.transition(t);
}

exports.d3AttrsFn = function(attrs, selection) {
    return selection.attr(attrs);
}

exports.d3DataAFn = function (data, selection) {
    return selection(data);
}

exports.d3DataHFn = function (data, selection) {
    return selection(data);
}

// core D3 function dummies, to be replaced with actual D3 calls later
var d3 = {
    select: function (selector) {
                console.log("d3Select: " + selector);
            }

}
// other dummy / temporary stuff for development
exports.emptySelection = {}
exports.dummyD3Fn   = function (selection) {
    console.log("dummyD3Fn: " + selection);
    return exports.emptySelection;
}
