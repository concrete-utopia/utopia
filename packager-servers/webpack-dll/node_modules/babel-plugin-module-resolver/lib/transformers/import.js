'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = transformImportCall;
function transformImportCall(t, nodePath, mapper, state, cwd) {
  var source = nodePath.get('source');
  if (source.type === 'StringLiteral') {
    var modulePath = mapper(source.node.value, state.file.opts.filename, state.opts, cwd);
    if (modulePath) {
      source.replaceWith(t.stringLiteral(modulePath));
    }
  }
}