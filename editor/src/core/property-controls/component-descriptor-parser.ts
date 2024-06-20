import * as BabelParser from '@babel/parser'
import traverse from '@babel/traverse'
import type { Bounds } from 'utopia-vscode-common'
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

  const componentBoundsByModule: ComponentBoundsByModule = {}

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
      if (declaration.type !== 'Identifier') {
        return
      }

      const variableName = declaration.name
      const componentsNode = variableDeclarations[variableName]
      if (componentsNode?.type !== 'ObjectExpression') {
        return
      }

      componentsNode.properties.forEach((module: any) => {
        if (module.type !== 'ObjectProperty' || module.key.type !== 'StringLiteral') {
          return
        }

        const moduleName = module.key.value
        if (componentBoundsByModule[moduleName] == null) {
          componentBoundsByModule[moduleName] = {}
        }

        const moduleValue = resolveVariable(module.value)
        if (moduleValue.type !== 'ObjectExpression') {
          return
        }

        moduleValue.properties.forEach((component: any) => {
          if (component.type !== 'ObjectProperty' || component.key.type !== 'Identifier') {
            return
          }

          const componentName = component.key.name
          const { loc } = component
          if (loc != null) {
            componentBoundsByModule[moduleName][componentName] = {
              startLine: loc.start.line - 1,
              startCol: loc.start.column - 1,
              endLine: loc.end.line - 1,
              endCol: loc.end.column - 1,
            }
          }
        })
      })
    },
  })
  return componentBoundsByModule
}
