import * as TS from 'typescript'

interface NamedImport {
  key: string
  name: string
}

type DeclarationName = TS.Identifier | TS.ObjectBindingPattern

interface DeclarationNames {
  namedDeclaration: DeclarationName | null
  starDeclaration: DeclarationName | null
}

function declarationNameForNamedImports(namedImports: Array<NamedImport>): DeclarationName {
  const bindingElements = namedImports.map(({ key, name }) =>
    TS.createBindingElement(
      undefined,
      TS.createIdentifier(key),
      TS.createIdentifier(name),
      undefined,
    ),
  )

  return TS.createObjectBindingPattern(bindingElements)
}

function declarationNameForStarImport(starImport: string): DeclarationName {
  return TS.createIdentifier(starImport)
}

function variableNamesFromImportClause(
  sourceFile: TS.SourceFile,
  clause: TS.ImportClause,
): DeclarationNames {
  let namedImports: Array<NamedImport> = []
  let starImport: string | null = null

  if (clause.name != null) {
    // import thing from './thing'
    namedImports.push({ key: 'default', name: clause.name.getText(sourceFile) })
  }

  if (clause.namedBindings != null) {
    const namedBindings = clause.namedBindings
    if (TS.isNamedImports(namedBindings)) {
      // import { thing as stuff } from './thing'
      for (const element of namedBindings.elements) {
        const keyIdentifier = element.propertyName ?? element.name
        const key = keyIdentifier.getText(sourceFile)
        const name = element.name.getText(sourceFile)
        namedImports.push({ key: key, name: name })
      }
    } else {
      // import * as thing from './thing'
      starImport = namedBindings.name.getText(sourceFile)
    }
  }

  const namedDeclaration =
    namedImports.length === 0 ? null : declarationNameForNamedImports(namedImports)
  const starDeclaration = starImport == null ? null : declarationNameForStarImport(starImport)

  return {
    namedDeclaration: namedDeclaration,
    starDeclaration: starDeclaration,
  }
}

function variableStatementsForImportClause(
  sourceFile: TS.SourceFile,
  importClause: TS.ImportClause,
  loadedModule: TS.ObjectLiteralExpression,
): Array<TS.VariableStatement> {
  let variableStatements: Array<TS.VariableStatement> = []

  const pushVariable = (name: DeclarationName) =>
    variableStatements.push(
      TS.createVariableStatement(
        undefined,
        TS.createVariableDeclarationList(
          [TS.createVariableDeclaration(name, undefined, loadedModule)],
          TS.NodeFlags.Const,
        ),
      ),
    )

  const { namedDeclaration, starDeclaration } = variableNamesFromImportClause(
    sourceFile,
    importClause,
  )

  if (namedDeclaration != null) {
    pushVariable(namedDeclaration)
  }

  if (starDeclaration != null) {
    pushVariable(starDeclaration)
  }

  return variableStatements
}

function applyLoaderToMatchingImport(
  sourceFile: TS.SourceFile,
  importDeclaration: TS.ImportDeclaration,
  loadedModule: TS.ObjectLiteralExpression,
): TS.VisitResult<TS.Node> {
  if (importDeclaration.importClause != null) {
    // import _something_ from _some file_
    return variableStatementsForImportClause(
      sourceFile,
      importDeclaration.importClause,
      loadedModule,
    )
  } else {
    // import _some file_ => (loadedModule)['default']
    return TS.createExpressionStatement(
      TS.createElementAccess(TS.createParen(loadedModule), TS.createStringLiteral('default')),
    )
  }
}

function applyLoaderToMatchingRequire(
  loadedModuleDefaultExport: TS.Expression,
): TS.VisitResult<TS.Node> {
  return loadedModuleDefaultExport
}

export type ImportMatcher = (importPath: string) => boolean
export type ModuleLoader = (
  sourceFile: TS.SourceFile,
  importPath: string,
) => TS.ObjectLiteralExpression
export type ModuleLoaderDefaultExport = (
  sourceFile: TS.SourceFile,
  importPath: string,
) => TS.Expression

function loaderVisitor(
  sourceFile: TS.SourceFile,
  context: TS.TransformationContext,
  matcher: ImportMatcher,
  moduleLoader: ModuleLoader,
  moduleLoaderDefaultExport: ModuleLoaderDefaultExport,
): TS.Visitor {
  const visitor = (node: TS.Node): TS.VisitResult<TS.Node> => {
    let newNodes: TS.VisitResult<TS.Node> = undefined

    if (TS.isImportDeclaration(node)) {
      const moduleSpecifier = node.moduleSpecifier.getText(sourceFile) // The string here will include the quotation marks
      const importPath = moduleSpecifier.slice(1, -1)

      if (matcher(importPath)) {
        const loadedModule = moduleLoader(sourceFile, importPath)

        newNodes = applyLoaderToMatchingImport(sourceFile, node, loadedModule)
      }
    }

    // requires

    // exports

    if (newNodes == undefined) {
      return TS.visitEachChild(node, visitor, context)
    } else {
      // FIXME Update source maps
      // TS.setSourceMapRange(newNodes, TS.getSourceMapRange(node))
      return newNodes
    }
  }

  // Update source map range

  return visitor
}

export function createLoaderTransformer(
  context: TS.TransformationContext,
  matcher: ImportMatcher,
  moduleLoader: ModuleLoader,
  moduleLoaderDefaultExport: ModuleLoaderDefaultExport,
): TS.Transformer<TS.SourceFile> {
  return (sourceFile: TS.SourceFile) =>
    TS.visitNode(
      sourceFile,
      loaderVisitor(sourceFile, context, matcher, moduleLoader, moduleLoaderDefaultExport),
    )
}
