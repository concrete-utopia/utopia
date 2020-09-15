'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

exports.default = function (node) {
  var comments = node.trailingComments || [];

  return !!comments.find(function (_ref) {
    var value = _ref.value;
    return value.trim() === 'remove-proptypes';
  });
};