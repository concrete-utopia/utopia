import { TopLevelElement } from '../../core/shared/element-template'
import { Imports, isParseSuccess, isTextFile } from '../../core/shared/project-file-types'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../assets'

export function importedFromWhere(
  originFilePath: string,
  variableName: string,
  topLevelElements: Array<TopLevelElement>,
  importsToSearch: Imports,
): string | null {
  for (const topLevelElement of topLevelElements) {
    switch (topLevelElement.type) {
      case 'UTOPIA_JSX_COMPONENT':
        if (topLevelElement.name === variableName) {
          return originFilePath
        }
        break
      case 'ARBITRARY_JS_BLOCK':
        if (topLevelElement.definedWithin.includes(variableName)) {
          return originFilePath
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
      return importSource
    }
    if (specificImport.importedWithName === variableName) {
      return importSource
    }
    if (specificImport.importedFromWithin.some((within) => within.alias === variableName)) {
      return importSource
    }
  }
  return null
}

/*
interface OriginatingNameResult {
  filePath: string
  variableName: string
}

export function findOriginatingName(
  projectContents: ProjectContentTreeRoot,
  originFilePath: string,
  originVariableName: string,
): OriginatingNameResult | null {
  const file = getContentsTreeFileFromString(projectContents, originFilePath)
  if (file == null) {
    return null
  } else if (isTextFile(file)) {
    const fileParseResult = file.fileContents.parsed
    if (isParseSuccess(fileParseResult)) {
      const importedFromResult = importedFromWhere(
        originFilePath,
        originVariableName,
        fileParseResult.topLevelElements,
        fileParseResult.imports,
      )

    } else {
      return null
    }
  } else {
    return null
  }
}
*/
