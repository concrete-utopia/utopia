import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import type { UtopiaJSXComponent } from '../../core/shared/element-template'
import { isUtopiaJSXComponent } from '../../core/shared/element-template'
import type {
  ImportDetails,
  Imports,
  ParsedTextFile,
  ParseSuccess,
} from '../../core/shared/project-file-types'
import {
  foldParsedTextFile,
  importAlias,
  importDetails,
} from '../../core/shared/project-file-types'
import { emptyImports } from '../../core/workers/common/project-file-utils'
import { StoryboardFilePath } from './store/editor-state'

interface ExportedComponentDetail {
  importsToAdd: Imports
  listingName: string
}

function exportedComponentDetail(
  importsToAdd: Imports,
  listingName: string,
): ExportedComponentDetail {
  return {
    importsToAdd: importsToAdd,
    listingName: listingName,
  }
}

type ExportedComponentImports = Array<ExportedComponentDetail>

function pathLastPartWithoutExtension(path: string): string {
  const splitPath = path.split('/')
  const lastPart = splitPath[splitPath.length - 1]
  const splitByFullStop = lastPart.split('.')
  return splitByFullStop[0]
}

export function getExportedComponentImports(
  originatingPath: string,
  fullPath: string,
  textFile: ParsedTextFile,
): ExportedComponentImports | null {
  return foldParsedTextFile(
    () => {
      return null
    },
    (success: ParseSuccess) => {
      const pathLastPart = pathLastPartWithoutExtension(fullPath)
      let result: ExportedComponentImports = []

      function isStoryboard(component: UtopiaJSXComponent): boolean {
        return fullPath === StoryboardFilePath && component.name === BakedInStoryboardVariableName
      }

      // All the heavy lifting for what to add happens in here.
      function addToResult(
        elementMatchesName: string | null,
        listingName: string,
        importDetailsToAdd: ImportDetails,
      ): void {
        for (const topLevelElement of success.topLevelElements) {
          if (
            isUtopiaJSXComponent(topLevelElement) &&
            topLevelElement.name === elementMatchesName &&
            !isStoryboard(topLevelElement)
          ) {
            // Don't add an import if this is from the same file.
            const importsToAdd =
              originatingPath === fullPath ? emptyImports() : { [fullPath]: importDetailsToAdd }
            result.push(exportedComponentDetail(importsToAdd, listingName))
          }
        }
      }

      for (const exportDetail of success.exportsDetail) {
        switch (exportDetail.type) {
          case 'EXPORT_DEFAULT_FUNCTION_OR_CLASS':
            addToResult(exportDetail.name, '(default)', importDetails(pathLastPart, [], null))
            break
          case 'EXPORT_CLASS':
            addToResult(
              exportDetail.className,
              exportDetail.className,
              importDetails(null, [importAlias(exportDetail.className)], null),
            )
            break
          case 'EXPORT_FUNCTION':
            addToResult(
              exportDetail.functionName,
              exportDetail.functionName,
              importDetails(null, [importAlias(exportDetail.functionName)], null),
            )
            break
          case 'EXPORT_VARIABLES':
          case 'EXPORT_DESTRUCTURED_ASSIGNMENT':
          case 'REEXPORT_VARIABLES':
            for (const exportVar of exportDetail.variables) {
              const exportName = exportVar.variableAlias ?? exportVar.variableName
              addToResult(
                exportVar.variableName,
                exportName === 'default' ? '(default)' : exportName,
                importDetails(
                  null,
                  exportName === 'default' ? [] : [importAlias(exportVar.variableName)],
                  null,
                ),
              )
            }
            break
          case 'EXPORT_IDENTIFIER':
            addToResult(
              exportDetail.name,
              exportDetail.name,
              importDetails(null, [importAlias(exportDetail.name)], null),
            )
            break
          case 'REEXPORT_WILDCARD':
            break
          case 'EXPORT_VARIABLES_WITH_MODIFIER':
            for (const exportName of exportDetail.variables) {
              addToResult(
                exportName,
                exportName === 'default' ? '(default)' : exportName,
                importDetails(
                  null,
                  exportName === 'default' ? [] : [importAlias(exportName)],
                  null,
                ),
              )
            }
            break
          default:
            const _exhaustiveCheck: never = exportDetail
            throw new Error(`Unhandled type ${JSON.stringify(exportDetail)}`)
        }
      }

      return result
    },
    () => {
      return null
    },
    textFile,
  )
}
