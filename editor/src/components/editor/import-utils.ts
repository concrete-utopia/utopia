import { resolveModulePath } from '../../core/es-modules/package-manager/module-resolution'
import { foldEither } from '../../core/shared/either'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { TopLevelElement } from '../../core/shared/element-template'
import {
  importAlias,
  importDetails,
  Imports,
  isParseSuccess,
  isTextFile,
  NodeModules,
} from '../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../assets'

interface SameFileOrigin {
  type: 'SAME_FILE_ORIGIN'
  filePath: string
}

function sameFileOrigin(filePath: string): SameFileOrigin {
  return {
    type: 'SAME_FILE_ORIGIN',
    filePath: filePath,
  }
}

interface ImportedOrigin {
  type: 'IMPORTED_ORIGIN'
  filePath: string
  exportedName: string | null
}

function importedOrigin(filePath: string, exportedName: string | null): ImportedOrigin {
  return {
    type: 'IMPORTED_ORIGIN',
    filePath: filePath,
    exportedName: exportedName,
  }
}

type ImportedFromWhereResult = SameFileOrigin | ImportedOrigin

export function importedFromWhere(
  originFilePath: string,
  variableName: string,
  topLevelElements: Array<TopLevelElement>,
  importsToSearch: Imports,
): ImportedFromWhereResult | null {
  for (const topLevelElement of topLevelElements) {
    switch (topLevelElement.type) {
      case 'UTOPIA_JSX_COMPONENT':
        if (topLevelElement.name === variableName) {
          return sameFileOrigin(originFilePath)
        }
        break
      case 'ARBITRARY_JS_BLOCK':
        if (topLevelElement.definedWithin.includes(variableName)) {
          return sameFileOrigin(originFilePath)
        }
        break
      case 'UNPARSED_CODE':
        break
      case 'IMPORT_STATEMENT':
        break
      default:
        const _exhaustiveCheck: never = topLevelElement
        throw new Error(`Unhandled element type ${JSON.stringify(topLevelElement)}`)
    }
  }
  for (const importSource of Object.keys(importsToSearch)) {
    const specificImport = importsToSearch[importSource]
    if (specificImport.importedAs === variableName) {
      return importedOrigin(importSource, null)
    }
    if (specificImport.importedWithName === variableName) {
      return importedOrigin(importSource, null)
    }
    for (const fromWithin of specificImport.importedFromWithin) {
      if (fromWithin.alias === variableName) {
        return importedOrigin(importSource, fromWithin.name)
      }
    }
  }
  return null
}

export function getTopLevelName(
  fromWhere: ImportedFromWhereResult,
  originalTopLevelName: string | null,
): string | null {
  switch (fromWhere.type) {
    case 'IMPORTED_ORIGIN':
      return fromWhere.exportedName
    case 'SAME_FILE_ORIGIN':
      return originalTopLevelName
    default:
      const _exhaustiveCheck: never = fromWhere
      throw new Error(`Unhandled type ${JSON.stringify(fromWhere)}`)
  }
}

export function getImportsFor(
  currentImports: Imports,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  importOrigin: string,
  importedName: string,
): Imports {
  for (const fileKey of Object.keys(currentImports)) {
    const details = currentImports[fileKey]
    const importPath = resolveModulePath(projectContents, nodeModules, importOrigin, fileKey)
    const resolvedImportPath = foldEither(
      (failure) => {
        throw new Error(`Could not resolve ${fileKey} to a path because: ${failure}`)
      },
      (success) => {
        return success
      },
      importPath,
    )

    if (details.importedAs === importedName) {
      return { [resolvedImportPath]: importDetails(null, [], importedName) }
    }
    if (details.importedWithName === importedName) {
      return { [resolvedImportPath]: importDetails(importedName, [], null) }
    }
    for (const fromWithin of details.importedFromWithin) {
      if (fromWithin.alias === importedName) {
        return {
          [resolvedImportPath]: importDetails(
            null,
            [importAlias(importedName, importedName)],
            null,
          ),
        }
      }
    }
  }

  return emptyImports()
}
