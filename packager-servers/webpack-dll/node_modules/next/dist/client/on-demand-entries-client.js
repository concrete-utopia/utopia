'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _promise = require('babel-runtime/core-js/promise');

var _promise2 = _interopRequireDefault(_promise);

var _regenerator = require('babel-runtime/regenerator');

var _regenerator2 = _interopRequireDefault(_regenerator);

var _asyncToGenerator2 = require('babel-runtime/helpers/asyncToGenerator');

var _asyncToGenerator3 = _interopRequireDefault(_asyncToGenerator2);

var _router = require('../lib/router');

var _router2 = _interopRequireDefault(_router);

var _unfetch = require('unfetch');

var _unfetch2 = _interopRequireDefault(_unfetch);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/* global location */

exports.default = function () {
  var ping = function () {
    var _ref = (0, _asyncToGenerator3.default)(_regenerator2.default.mark(function _callee() {
      var url, res, payload;
      return _regenerator2.default.wrap(function _callee$(_context) {
        while (1) {
          switch (_context.prev = _context.next) {
            case 0:
              _context.prev = 0;
              url = '/_next/on-demand-entries-ping?page=' + _router2.default.pathname;
              _context.next = 4;
              return (0, _unfetch2.default)(url);

            case 4:
              res = _context.sent;
              _context.next = 7;
              return res.json();

            case 7:
              payload = _context.sent;

              if (payload.invalid) {
                location.reload();
              }
              _context.next = 14;
              break;

            case 11:
              _context.prev = 11;
              _context.t0 = _context['catch'](0);

              console.error('Error with on-demand-entries-ping: ' + _context.t0.message);

            case 14:
            case 'end':
              return _context.stop();
          }
        }
      }, _callee, this, [[0, 11]]);
    }));

    return function ping() {
      return _ref.apply(this, arguments);
    };
  }();

  var runPinger = function () {
    var _ref2 = (0, _asyncToGenerator3.default)(_regenerator2.default.mark(function _callee2() {
      return _regenerator2.default.wrap(function _callee2$(_context2) {
        while (1) {
          switch (_context2.prev = _context2.next) {
            case 0:
              if (!true) {
                _context2.next = 7;
                break;
              }

              _context2.next = 3;
              return new _promise2.default(function (resolve) {
                return setTimeout(resolve, 5000);
              });

            case 3:
              _context2.next = 5;
              return ping();

            case 5:
              _context2.next = 0;
              break;

            case 7:
            case 'end':
              return _context2.stop();
          }
        }
      }, _callee2, this);
    }));

    return function runPinger() {
      return _ref2.apply(this, arguments);
    };
  }();

  _router2.default.ready(function () {
    _router2.default.router.events.on('routeChangeComplete', ping);
  });

  runPinger().catch(function (err) {
    console.error(err);
  });
};