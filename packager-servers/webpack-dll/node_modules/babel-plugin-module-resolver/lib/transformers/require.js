'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = transformRequireCall;
function transformRequireCall(t, nodePath, mapper, state, cwd) {
  var calleePath = nodePath.get('callee');
  if (!t.isIdentifier(calleePath.node, { name: 'require' }) && !(t.isMemberExpression(calleePath.node) && t.isIdentifier(calleePath.node.object, { name: 'require' }))) {
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