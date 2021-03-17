import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { isUtopiaJSXComponent, UtopiaJSXComponent } from '../../core/shared/element-template'
import {
  foldParsedTextFile,
  importAlias,
  ImportDetails,
  importDetails,
  Imports,
  ParsedTextFile,
  ParseSuccess,
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
        elementMatchesName: string,
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

      // Default export for the entire file.
      if (success.exportsDetail.defaultExport != null) {
        const defaultExport = success.exportsDetail.defaultExport
        switch (defaultExport.type) {
          case 'EXPORT_DEFAULT_NAMED':
            addToResult(defaultExport.name, '(default)', importDetails(pathLastPart, [], null))
            break
          case 'EXPORT_DEFAULT_MODIFIER':
            addToResult(defaultExport.name, '(default)', importDetails(pathLastPart, [], null))
            break
        }
      }

      // Handle exports where the component is against a key in the exports object.
      for (const namedExportKey of Object.keys(success.exportsDetail.namedExports)) {
        const namedExport = success.exportsDetail.namedExports[namedExportKey]
        switch (namedExport.type) {
          case 'EXPORT_DETAIL_NAMED':
            addToResult(
              namedExport.name,
              namedExportKey,
              importDetails(null, [importAlias(namedExportKey)], null),
            )
            break
          case 'EXPORT_DETAIL_MODIFIER':
            addToResult(
              namedExportKey,
              namedExportKey,
              importDetails(null, [importAlias(namedExportKey)], null),
            )
            break
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
