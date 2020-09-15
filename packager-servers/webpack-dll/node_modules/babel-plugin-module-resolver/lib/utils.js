'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.toPosixPath = toPosixPath;
exports.toLocalPath = toLocalPath;
exports.replaceExtension = replaceExtension;

var _path = require('path');

var _path2 = _interopRequireDefault(_path);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function toPosixPath(modulePath) {
  return modulePath.replace(/\\/g, '/');
}

function toLocalPath(p) {
  return p.replace(/\/index$/, '') // remove trailing /index
  .replace(/^(?!\.)/, './'); // insert `./` to make it a local path
}

function replaceExtension(p, ext) {
  var filename = _path2.default.basename(p, _path2.default.extname(p)) + ext;
  return _path2.default.join(_path2.default.dirname(p), filename);
}