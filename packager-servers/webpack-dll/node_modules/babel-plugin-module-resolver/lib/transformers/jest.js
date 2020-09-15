'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = transformJestCalls;
function transformJestCalls(t, nodePath, mapper, state, cwd) {
  var calleePath = nodePath.get('callee');

  var jestMethods = ['genMockFromModule', 'mock', 'unmock', 'doMock', 'dontMock'];

  if (!(t.isMemberExpression(calleePath.node) && t.isIdentifier(calleePath.node.object, { name: 'jest' }) && jestMethods.some(function (methodName) {
    return t.isIdentifier(calleePath.node.property, { name: methodName });
  }))) {
    return;
  }

  var args = nodePath.get('arguments');
  if (!args.length) {
    return;
  }

  var moduleArg = args[0];
  if (moduleArg.node.type === 'StringLiteral') {
    var modulePath = mapper(moduleArg.node.value, state.file.opts.filename, state.opts, cwd);
    if (modulePath) {
      moduleArg.replaceWith(t.stringLiteral(modulePath));
    }
  }
}