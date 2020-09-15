'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

exports.default = function (_ref) {
  var t = _ref.types;

  return {
    visitor: {
      JSXOpeningElement: function (path, _ref2) {
        var file = _ref2.file;

        file.set('hasJSX', true);
      },


      Program: {
        enter: function (path, _ref3) {
          var file = _ref3.file;

          file.set('hasJSX', false);
        },
        exit: function (_ref4, _ref5) {
          var node = _ref4.node;
          var scope = _ref4.scope;
          var file = _ref5.file;

          if (!(file.get('hasJSX') && !scope.hasBinding('React'))) {
            return;
          }

          var reactImportDeclaration = t.importDeclaration([t.importDefaultSpecifier(t.identifier('React'))], t.stringLiteral('react'));

          node.body.unshift(reactImportDeclaration);
        }
      }
    }
  };
};