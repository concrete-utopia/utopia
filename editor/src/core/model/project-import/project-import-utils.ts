import { getProjectFileByFilePath, isProjectContentFile } from '../../../components/assets'
import type { Imports } from '../../../core/shared/project-file-types'
import {
  forEachParseSuccess,
  isEmptyImportDetails,
  isTextFile,
} from '../../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import { getFilePathMappings } from '../project-file-utils'
import type { ComponentToImport } from './storyboard-utils'
import { namedComponentToImport, PossiblyMainComponentNames } from './storyboard-utils'
import { mergeImports } from '../../../core/workers/common/project-file-utils'
import { absolutePathFromRelativePath } from '../../../utils/path-utils'

export interface CreationDataFromProject {
  maybeRootElementId?: string
  maybeRootElementClass?: string
  componentsToImport: ComponentToImport[]
  extraImports: Imports
}

export function getCreationDataFromProject(
  projectContents: ProjectContentTreeRoot,
): CreationDataFromProject {
  const componentsToImport: ComponentToImport[] = []
  let extraImports: Imports = {}
  const creationDataFromHtml = getCreationDataFromIndexHtmlFile(projectContents)
  if (creationDataFromHtml.maybeMainScriptFilePath != null) {
    const mainScriptFile = getProjectFileByFilePath(
      projectContents,
      creationDataFromHtml.maybeMainScriptFilePath,
    )

    if (mainScriptFile != null && isTextFile(mainScriptFile)) {
      forEachParseSuccess((success) => {
        // check for import candidates
        function toAbsolutePath(importSource: string): string {
          if (creationDataFromHtml.maybeMainScriptFilePath != null) {
            return absolutePathFromRelativePath(
              creationDataFromHtml.maybeMainScriptFilePath,
              false,
              importSource,
            )
          }
          return importSource
        }
        Object.entries(success.imports).forEach(([importSource, importDetails]) => {
          // "import App from '...'"
          if (
            importDetails.importedWithName != null &&
            PossiblyMainComponentNames.includes(importDetails.importedWithName)
          ) {
            componentsToImport.push(
              namedComponentToImport(
                toAbsolutePath(importSource),
                true,
                'default',
                importDetails.importedWithName,
              ),
            )
          }
          // "import { App } from '...'"
          importDetails.importedFromWithin.forEach((importAlias) => {
            if (PossiblyMainComponentNames.includes(importAlias.alias)) {
              componentsToImport.push(
                namedComponentToImport(
                  toAbsolutePath(importSource),
                  true,
                  importAlias.name,
                  importAlias.alias,
                ),
              )
            }
          })
          // "import '...'"
          if (isEmptyImportDetails(importDetails)) {
            extraImports = mergeImports(
              creationDataFromHtml.maybeMainScriptFilePath ?? '',
              getFilePathMappings(projectContents),
              extraImports,
              {
                [toAbsolutePath(importSource)]: importDetails,
              },
            ).imports
          }
        })
      }, mainScriptFile.fileContents.parsed)
    }
  }
  return {
    maybeRootElementId: creationDataFromHtml.maybeRootElementId,
    maybeRootElementClass: creationDataFromHtml.maybeRootElementClass,
    componentsToImport: componentsToImport,
    extraImports: extraImports,
  }
}

function getCreationDataFromIndexHtmlFile(projectContents: ProjectContentTreeRoot): {
  maybeRootElementId?: string
  maybeRootElementClass?: string
  maybeMainScriptFilePath?: string
} {
  const indexHtmlFile = projectContents['index.html']
  if (
    indexHtmlFile != null &&
    isProjectContentFile(indexHtmlFile) &&
    isTextFile(indexHtmlFile.content)
  ) {
    // try to parse the html file
    const doc = parseHtml(indexHtmlFile.content.fileContents.code)
    if (doc != null) {
      const rootElement = getRootElement(doc)
      const mainScriptElement = getMainScriptElement(doc)
      return {
        maybeRootElementId: rootElement?.id,
        maybeRootElementClass: rootElement?.className,
        maybeMainScriptFilePath: mainScriptElement?.getAttribute('src') ?? undefined,
      }
    }
  }
  return {}
}

function parseHtml(html: string): Document | null {
  try {
    const parser = new DOMParser()
    const doc = parser.parseFromString(html, 'text/html')
    return doc
  } catch (e) {
    return null
  }
}

function getRootElement(doc: Document): Element | null {
  const body = doc.body
  if (body != null) {
    return body.firstElementChild
  }
  return null
}

function getMainScriptElement(doc: Document): Element | null {
  const body = doc.body
  if (body != null) {
    const scriptElements = body.querySelectorAll('script[type="module"]')
    if (scriptElements.length > 0) {
      return scriptElements[0]
    }
  }
  return null
}
