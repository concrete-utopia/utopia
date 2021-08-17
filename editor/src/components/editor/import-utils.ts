import { TopLevelElement } from '../../core/shared/element-template'
import { Imports, isParseSuccess, isTextFile } from '../../core/shared/project-file-types'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../assets'

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
