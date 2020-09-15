'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = mapToRelative;

var _path = require('path');

var _path2 = _interopRequireDefault(_path);

var _utils = require('./utils');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function resolve(cwd, filename) {
  if (_path2.default.isAbsolute(filename)) return filename;
  return _path2.default.resolve(cwd, filename);
}

function mapToRelative(cwd, currentFile, module) {
  var from = _path2.default.dirname(currentFile);
  var to = _path2.default.normalize(module);

  from = resolve(cwd, from);
  to = resolve(cwd, to);

  var moduleMapped = _path2.default.relative(from, to);
  return (0, _utils.toPosixPath)(moduleMapped);
}