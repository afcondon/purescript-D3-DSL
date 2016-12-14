/* global exports */
"use strict"

exports.invisible = "1e-6"
exports.opaque    = "1"
exports.getAttrN  = getAttr
exports.getAttrS  = getAttr

function getAttr(selection, attr) {
    return selection.attr(attr);
}
