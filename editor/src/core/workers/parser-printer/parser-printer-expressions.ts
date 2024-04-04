import * as TS from 'typescript-for-the-editor'

export function identifyValuesDefinedInNode(
  sourceFile: TS.SourceFile,
  node: TS.Node,
): Array<string> {
  // TODO: Handle destructuring.
  let names: Array<string> = []
  if (TS.isVariableStatement(node)) {
    for (const variableDeclaration of node.declarationList.declarations) {
      if (variableDeclaration.initializer != null) {
        names.push(variableDeclaration.name.getText(sourceFile))
      }
    }
  } else if (TS.isFunctionLike(node)) {
    if (node.name != null) {
      names.push(node.name.getText(sourceFile))
    }
  } else if (TS.isClassDeclaration(node)) {
    if (node.name != null) {
      names.push(node.name.getText(sourceFile))
    }
  }
  return names
}
