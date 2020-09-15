'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _set = require('babel-runtime/core-js/set');

var _set2 = _interopRequireDefault(_set);

var _regenerator = require('babel-runtime/regenerator');

var _regenerator2 = _interopRequireDefault(_regenerator);

var _getIterator2 = require('babel-runtime/core-js/get-iterator');

var _getIterator3 = _interopRequireDefault(_getIterator2);

var _asyncToGenerator2 = require('babel-runtime/helpers/asyncToGenerator');

var _asyncToGenerator3 = _interopRequireDefault(_asyncToGenerator2);

var isFile = function () {
  var _ref2 = (0, _asyncToGenerator3.default)(_regenerator2.default.mark(function _callee2(p) {
    var stat, realpath;
    return _regenerator2.default.wrap(function _callee2$(_context2) {
      while (1) {
        switch (_context2.prev = _context2.next) {
          case 0:
            stat = void 0;
            _context2.prev = 1;
            _context2.next = 4;
            return _fs2.default.stat(p);

          case 4:
            stat = _context2.sent;
            _context2.next = 12;
            break;

          case 7:
            _context2.prev = 7;
            _context2.t0 = _context2['catch'](1);

            if (!(_context2.t0.code === 'ENOENT')) {
              _context2.next = 11;
              break;
            }

            return _context2.abrupt('return', false);

          case 11:
            throw _context2.t0;

          case 12:
            _context2.next = 14;
            return getTrueFilePath(p);

          case 14:
            realpath = _context2.sent;

            if (!(p !== realpath)) {
              _context2.next = 17;
              break;
            }

            return _context2.abrupt('return', false);

          case 17:
            return _context2.abrupt('return', stat.isFile() || stat.isFIFO());

          case 18:
          case 'end':
            return _context2.stop();
        }
      }
    }, _callee2, this, [[1, 7]]);
  }));

  return function isFile(_x2) {
    return _ref2.apply(this, arguments);
  };
}();

// This is based on the stackoverflow answer: http://stackoverflow.com/a/33139702/457224
// We assume we'll get properly normalized path names as p


var getTrueFilePath = function () {
  var _ref3 = (0, _asyncToGenerator3.default)(_regenerator2.default.mark(function _callee3(p) {
    var fsPathNormalized, pathRoot, noDrivePath, result;
    return _regenerator2.default.wrap(function _callee3$(_context3) {
      while (1) {
        switch (_context3.prev = _context3.next) {
          case 0:
            fsPathNormalized = p;
            // OSX: HFS+ stores filenames in NFD (decomposed normal form) Unicode format,
            // so we must ensure that the input path is in that format first.

            if (process.platform === 'darwin') fsPathNormalized = fsPathNormalized.normalize('NFD');

            // !! Windows: Curiously, the drive component mustn't be part of a glob,
            // !! otherwise glob.sync() will invariably match nothing.
            // !! Thus, we remove the drive component and instead pass it in as the 'cwd'
            // !! (working dir.) property below.
            pathRoot = (0, _path.parse)(fsPathNormalized).root;
            noDrivePath = fsPathNormalized.slice(Math.max(pathRoot.length - 1, 0));

            // Perform case-insensitive globbing (on Windows, relative to the drive /
            // network share) and return the 1st match, if any.
            // Fortunately, glob() with nocase case-corrects the input even if it is
            // a *literal* path.

            _context3.next = 6;
            return (0, _globPromise2.default)(noDrivePath, { nocase: true, cwd: pathRoot });

          case 6:
            result = _context3.sent;
            return _context3.abrupt('return', result[0]);

          case 8:
          case 'end':
            return _context3.stop();
        }
      }
    }, _callee3, this);
  }));

  return function getTrueFilePath(_x3) {
    return _ref3.apply(this, arguments);
  };
}();

exports.resolveFromList = resolveFromList;

var _path = require('path');

var _fs = require('mz/fs');

var _fs2 = _interopRequireDefault(_fs);

var _globPromise = require('glob-promise');

var _globPromise2 = _interopRequireDefault(_globPromise);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function () {
  var _ref = (0, _asyncToGenerator3.default)(_regenerator2.default.mark(function _callee(id) {
    var paths, _iteratorNormalCompletion, _didIteratorError, _iteratorError, _iterator, _step, p, err;

    return _regenerator2.default.wrap(function _callee$(_context) {
      while (1) {
        switch (_context.prev = _context.next) {
          case 0:
            paths = getPaths(id);
            _iteratorNormalCompletion = true;
            _didIteratorError = false;
            _iteratorError = undefined;
            _context.prev = 4;
            _iterator = (0, _getIterator3.default)(paths);

          case 6:
            if (_iteratorNormalCompletion = (_step = _iterator.next()).done) {
              _context.next = 15;
              break;
            }

            p = _step.value;
            _context.next = 10;
            return isFile(p);

          case 10:
            if (!_context.sent) {
              _context.next = 12;
              break;
            }

            return _context.abrupt('return', p);

          case 12:
            _iteratorNormalCompletion = true;
            _context.next = 6;
            break;

          case 15:
            _context.next = 21;
            break;

          case 17:
            _context.prev = 17;
            _context.t0 = _context['catch'](4);
            _didIteratorError = true;
            _iteratorError = _context.t0;

          case 21:
            _context.prev = 21;
            _context.prev = 22;

            if (!_iteratorNormalCompletion && _iterator.return) {
              _iterator.return();
            }

          case 24:
            _context.prev = 24;

            if (!_didIteratorError) {
              _context.next = 27;
              break;
            }

            throw _iteratorError;

          case 27:
            return _context.finish(24);

          case 28:
            return _context.finish(21);

          case 29:
            err = new Error('Cannot find module ' + id);

            err.code = 'ENOENT';
            throw err;

          case 32:
          case 'end':
            return _context.stop();
        }
      }
    }, _callee, this, [[4, 17, 21, 29], [22,, 24, 28]]);
  }));

  function resolve(_x) {
    return _ref.apply(this, arguments);
  }

  return resolve;
}();

function resolveFromList(id, files) {
  var paths = getPaths(id);
  var set = new _set2.default(files);
  var _iteratorNormalCompletion2 = true;
  var _didIteratorError2 = false;
  var _iteratorError2 = undefined;

  try {
    for (var _iterator2 = (0, _getIterator3.default)(paths), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
      var p = _step2.value;

      if (set.has(p)) return p;
    }
  } catch (err) {
    _didIteratorError2 = true;
    _iteratorError2 = err;
  } finally {
    try {
      if (!_iteratorNormalCompletion2 && _iterator2.return) {
        _iterator2.return();
      }
    } finally {
      if (_didIteratorError2) {
        throw _iteratorError2;
      }
    }
  }
}

function getPaths(id) {
  var i = _path.sep === '/' ? id : id.replace(/\//g, _path.sep);

  if (i.slice(-3) === '.js') return [i];
  if (i.slice(-5) === '.json') return [i];

  if (i[i.length - 1] === _path.sep) {
    return [i + 'index.js', i + 'index.json'];
  }

  return [i + '.js', (0, _path.join)(i, 'index.js'), i + '.json', (0, _path.join)(i, 'index.json')];
}