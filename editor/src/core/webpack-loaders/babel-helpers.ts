import * as BabelTypes from '@babel/types'
import { ImportSpecifier, ImportDefaultSpecifier, ImportNamespaceSpecifier } from '@babel/types'
import { fastForEach } from '../shared/utils'

interface NamedImport {
  key: string
  name: string
}

function declaratorLValForNamedImports(namedImports: Array<NamedImport>): BabelTypes.LVal {
  const properties = namedImports.map(({ key, name }) =>
    BabelTypes.objectProperty(BabelTypes.identifier(key), BabelTypes.identifier(name)),
  )

  return BabelTypes.objectPattern(properties)
}

function declaratorLValForStarImport(name: string): BabelTypes.LVal {
  return BabelTypes.identifier(name)
}

interface DeclaratorLVals {
  namedLVal: BabelTypes.LVal | null
  starLVal: BabelTypes.LVal | null
}

function declaratorLValsForImportSpecifiers(
  specifiers: Array<ImportSpecifier | ImportDefaultSpecifier | ImportNamespaceSpecifier>,
): DeclaratorLVals {
  let namedImports: Array<NamedImport> = []
  let starImport: string | null = null

  fastForEach(specifiers, (specifier) => {
    switch (specifier.type) {
      case 'ImportDefaultSpecifier':
        // import thing from './thing'
        namedImports.push({ key: 'default', name: specifier.local.name })
        break
      case 'ImportSpecifier':
        // import { thing as stuff } from './thing'
        namedImports.push({ key: specifier.imported.name, name: specifier.local.name })
        break
      case 'ImportNamespaceSpecifier':
        // import * as thing from './thing'
        starImport = specifier.local.name
        break
    }
  })

  const namedLVal = namedImports.length === 0 ? null : declaratorLValForNamedImports(namedImports)
  const starLVal = starImport == null ? null : declaratorLValForStarImport(starImport)

  return {
    namedLVal: namedLVal,
    starLVal: starLVal,
  }
}

export function variableDeclarationsForImport(
  importDeclaration: BabelTypes.ImportDeclaration,
  moduleObject: BabelTypes.ObjectExpression,
): Array<BabelTypes.VariableDeclaration> {
  // import thing from './thing' => const { default: thing } = moduleObject
  // import * as thing from './thing' => const thing = moduleObject
  // import { thing as stuff, otherThing } from './thing' => const {thing: stuff, otherThing} = moduleObject

  let variableDeclarations: Array<BabelTypes.VariableDeclaration> = []

  const pushVariable = (lval: BabelTypes.LVal) =>
    variableDeclarations.push(
      BabelTypes.variableDeclaration('const', [BabelTypes.variableDeclarator(lval, moduleObject)]),
    )

  const { namedLVal, starLVal } = declaratorLValsForImportSpecifiers(importDeclaration.specifiers)

  if (namedLVal != null) {
    pushVariable(namedLVal)
  }

  if (starLVal != null) {
    pushVariable(starLVal)
  }

  return variableDeclarations
}
