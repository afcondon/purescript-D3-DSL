/* global exports */
"use strict"

exports.d3DocSelectFn   = function (selector) { return d3.select(selector); }

exports.d3DocSelectAllFn   = function (selector) { return d3.selectAll(selector); }

exports.d3SelectFn = function (selector, selection) { return selection.select(selector); }

exports.d3SelectAllFn = function (selector, selection) { return selection.selectAll(selector); }

exports.d3MergeFn = function (merged, original) { return orignal.merge(merged); }

exports.d3AppendFn = function (element, selection) { return selection.append(element); }

exports.d3InsertFn = function (element, selection) { return selection.insert(element); }

exports.d3RemoveFn = function(selection) { return selection.remove(); }

exports.d3EnterFn = function(selection) { return selection.enter(); }

exports.d3ExitFn = function (selection) { return selection.exit(); }

exports.d3TransitionFn = function(t, selection) { return selection.transition(t); }

exports.d3AttrsFn = function(attrs, selection) { return selection.attr(attrs); }

exports.d3DataAFn = function (data, selection) { return selection.data(data); }

exports.d3DataHFn = function (data, selection) { return selection.data(data); }

exports.d3DataAIFn = function (data, indexfn, selection) { return selection.data(data, indexfn); }

exports.d3DataHIFn = function (data, indexfn, selection) { return selection.data(data, indexfn); }

// core D3 function dummies, to be replaced with actual D3 calls later
var d3 = {
    select: function (selector) {
                console.log("d3Select: " + selector);
            }

}
