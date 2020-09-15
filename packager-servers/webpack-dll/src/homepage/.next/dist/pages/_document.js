'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _getPrototypeOf = require('babel-runtime/core-js/object/get-prototype-of');

var _getPrototypeOf2 = _interopRequireDefault(_getPrototypeOf);

var _classCallCheck2 = require('babel-runtime/helpers/classCallCheck');

var _classCallCheck3 = _interopRequireDefault(_classCallCheck2);

var _createClass2 = require('babel-runtime/helpers/createClass');

var _createClass3 = _interopRequireDefault(_createClass2);

var _possibleConstructorReturn2 = require('babel-runtime/helpers/possibleConstructorReturn');

var _possibleConstructorReturn3 = _interopRequireDefault(_possibleConstructorReturn2);

var _inherits2 = require('babel-runtime/helpers/inherits');

var _inherits3 = _interopRequireDefault(_inherits2);

var _react = require('react');

var _react2 = _interopRequireDefault(_react);

var _document = require('next/dist/server/document.js');

var _document2 = _interopRequireDefault(_document);

var _server = require('styled-jsx/server');

var _server2 = _interopRequireDefault(_server);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var _jsxFileName = '/Users/enidemi/momentum/utopia/packager-servers/webpack-dll/src/homepage/pages/_document.js?entry';


var MyDocument = function (_Document) {
  (0, _inherits3.default)(MyDocument, _Document);

  function MyDocument() {
    (0, _classCallCheck3.default)(this, MyDocument);

    return (0, _possibleConstructorReturn3.default)(this, (MyDocument.__proto__ || (0, _getPrototypeOf2.default)(MyDocument)).apply(this, arguments));
  }

  (0, _createClass3.default)(MyDocument, [{
    key: 'render',
    value: function render() {
      return _react2.default.createElement('html', {
        __source: {
          fileName: _jsxFileName,
          lineNumber: 13
        }
      }, _react2.default.createElement(_document.Head, {
        __source: {
          fileName: _jsxFileName,
          lineNumber: 14
        }
      }, _react2.default.createElement('meta', { name: 'viewport', content: 'width=device-width, initial-scale=1', __source: {
          fileName: _jsxFileName,
          lineNumber: 15
        }
      }), _react2.default.createElement('title', {
        __source: {
          fileName: _jsxFileName,
          lineNumber: 16
        }
      }, 'NPM Packager Service'), _react2.default.createElement('link', {
        href: 'https://fonts.googleapis.com/css?family=Open+Sans:300,400',
        rel: 'stylesheet',
        __source: {
          fileName: _jsxFileName,
          lineNumber: 17
        }
      }), _react2.default.createElement('style', {
        dangerouslySetInnerHTML: {
          __html: '\n            html {\n              font-family: \'Open Sans\', sans-serif;\n              -webkit-font-smoothing: antialiased;\n              -moz-font-smoothing: antialiased;\n              -moz-osx-font-smoothing: grayscale;\n              font-smoothing: antialiased;\n              text-rendering: optimizeLegibility;\n              font-smooth: always;\n              -webkit-tap-highlight-color: transparent;\n              -webkit-touch-callout: none;\n\n              display: flex;\n              align-items: center;\n              justify-content: center;\n\n              line-height: 1.8;\n            }\n\n            @media screen and (max-width: 800px) {\n                html {\n                  display: block;\n                  padding: 16px;\n                }\n            }\n\n            body {\n              margin: 0;\n              background-color: #F4F4F4;\n              max-width: 800px;\n            }\n\n            p, code, div {\n              color: #5F6F86;\n            }\n\n            a {\n              color: rgba(52, 152, 219,1.0);\n              font-weight: 600;\n              text-decoration: none;\n            }\n          '
        },
        __source: {
          fileName: _jsxFileName,
          lineNumber: 21
        }
      })), _react2.default.createElement('body', {
        __source: {
          fileName: _jsxFileName,
          lineNumber: 68
        }
      }, _react2.default.createElement(_document.Main, {
        __source: {
          fileName: _jsxFileName,
          lineNumber: 69
        }
      }), _react2.default.createElement(_document.NextScript, {
        __source: {
          fileName: _jsxFileName,
          lineNumber: 70
        }
      })));
    }
  }], [{
    key: 'getInitialProps',
    value: function getInitialProps(_ref) {
      var renderPage = _ref.renderPage;

      var _renderPage = renderPage(),
          html = _renderPage.html,
          head = _renderPage.head;

      var styles = (0, _server2.default)();
      return { html: html, head: head, styles: styles };
    }
  }]);

  return MyDocument;
}(_document2.default);

exports.default = MyDocument;