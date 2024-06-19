import * as BabelParser from '@babel/parser'
import traverse from '@babel/traverse'
import type { Bounds } from 'utopia-vscode-common'
import { boundsInFile } from 'utopia-vscode-common'
import type { TextFileContentsWithPath } from './property-controls-local'

export type ComponentBoundsByModule = {
  [moduleName: string]: { [componentName: string]: Bounds }
}

export function generateComponentBounds(
  descriptorFile: TextFileContentsWithPath,
): ComponentBoundsByModule {
  const ast = BabelParser.parse(descriptorFile.file.code, {
    sourceType: 'module',
    plugins: ['jsx'],
  })

  const bounds: ComponentBoundsByModule = {}

  // Store variable declarations
  const variableDeclarations: Record<string, any> = {}
  // Traverse the AST to collect variable declarations
  traverse(ast, {
    VariableDeclarator(path) {
      const { id, init } = path.node
      if (id.type === 'Identifier' && init != null) {
        variableDeclarations[id.name] = init
      }
    },
  })

  // Function to resolve a variable to its object value
  function resolveVariable(node: any): any {
    if (node.type === 'Identifier' && variableDeclarations[node.name] != null) {
      return variableDeclarations[node.name]
    }
    return node
  }

  // Traverse the AST to find the default export and process the Components object
  traverse(ast, {
    ExportDefaultDeclaration(path) {
      const declaration = path.node.declaration
      if (declaration.type === 'Identifier') {
        const variableName = declaration.name
        const componentsNode = variableDeclarations[variableName]
        if (componentsNode?.type === 'ObjectExpression') {
          componentsNode.properties.forEach((prop: any) => {
            if (prop.type === 'ObjectProperty' && prop.key.type === 'StringLiteral') {
              const moduleName = prop.key.value
              if (bounds[moduleName] == null) {
                bounds[moduleName] = {}
              }
              const moduleValue = resolveVariable(prop.value)
              if (moduleValue.type === 'ObjectExpression') {
                moduleValue.properties.forEach((innerProp: any) => {
                  if (innerProp.type === 'ObjectProperty' && innerProp.key.type === 'Identifier') {
                    const componentName = innerProp.key.name
                    const { loc } = innerProp
                    if (loc != null) {
                      bounds[moduleName][componentName] = boundsInFile(
                        descriptorFile.path,
                        loc.start.line,
                        loc.start.column,
                        loc.end.line,
                        loc.end.column,
                      )
                    }
                  }
                })
              }
            }
          })
        }
      }
    },
  })
  return bounds
}
