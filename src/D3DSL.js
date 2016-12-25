/* global exports */
"use strict"

exports.invisible = "1e-6"
exports.opaque    = "1"
exports.getAttrN  = getAttr
exports.getAttrS  = getAttr
exports.runD3Fn   = function (selection) { console.log("runD3Fn: " + selection); }

function getAttr(selection, attr) {
    return selection.attr(attr);
}
