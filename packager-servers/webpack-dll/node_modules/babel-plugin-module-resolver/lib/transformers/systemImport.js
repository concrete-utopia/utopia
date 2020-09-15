'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = transformSystemImportCall;
function transformSystemImportCall(t, nodePath, mapper, state, cwd) {
  var calleePath = nodePath.get('callee');

  if (!(t.isMemberExpression(calleePath.node) && t.isIdentifier(calleePath.node.object, { name: 'System' }) && t.isIdentifier(calleePath.node.property, { name: 'import' }))) {
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