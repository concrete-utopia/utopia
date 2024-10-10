import type { NodeModules } from '../shared/project-file-types'
import type { FrameworkHooks } from './framework-hooks'
import { getProjectFileByFilePath, isProjectContentFile } from '../../components/assets'
import type { Imports } from '../shared/project-file-types'
import { forEachParseSuccess, isEmptyImportDetails, isTextFile } from '../shared/project-file-types'
import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import { getFilePathMappings } from '../model/project-file-utils'
import type { ComponentToImport, CreationDataFromProject } from '../model/storyboard-utils'
import { namedComponentToImport, PossiblyMainComponentNames } from '../model/storyboard-utils'
import { mergeImports } from '../workers/common/project-file-utils'
import { absolutePathFromRelativePath } from '../../utils/path-utils'
import type { FileLookupResult } from '../es-modules/package-manager/module-resolution-utils'
import { isResolveSuccess } from '../es-modules/package-manager/module-resolution-utils'
import { resolveModule } from '../es-modules/package-manager/module-resolution'
import { getMainScriptElement, getRootElement, parseHtml } from '../shared/dom-utils'

export class ViteFrameworkHooks implements FrameworkHooks {
  detect(projectContents: ProjectContentTreeRoot): boolean {
    return hasViteConfig(projectContents)
  }

  onProjectImport(projectContents: ProjectContentTreeRoot): CreationDataFromProject | null {
    return getCreationDataFromProject(projectContents)
  }

  onResolveModuleNotPresent(
    projectContents: ProjectContentTreeRoot,
    nodeModules: NodeModules,
    importOrigin: string,
    toImport: string,
  ): string | null {
    if (isInRootPath(toImport)) {
      const publicResolveResult = resolveModuleFromPublicDir(
        projectContents,
        nodeModules,
        importOrigin,
        toImport,
      )
      if (isResolveSuccess(publicResolveResult)) {
        return publicResolveResult.success.path
      }
    }
    return null
  }
}

const configFileNames = ['vite.config.ts', 'vite.config.js', 'vite.config.mjs']

function hasViteConfig(projectContents: ProjectContentTreeRoot): boolean {
  return configFileNames.some((fileName) => projectContents[fileName] != null)
}

export function getCreationDataFromProject(
  projectContents: ProjectContentTreeRoot,
): CreationDataFromProject {
  const componentsToImport: ComponentToImport[] = []
  let extraImports: Imports = {}
  const creationDataFromHtml = extractCreationDataFromMainHtmlFile(projectContents)
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

// TODO: read from vite config
const MAIN_HTML_FILE_NAME = 'index.html'

function extractCreationDataFromMainHtmlFile(projectContents: ProjectContentTreeRoot): {
  maybeRootElementId?: string
  maybeRootElementClass?: string
  maybeMainScriptFilePath?: string
} {
  const indexHtmlFile = projectContents[MAIN_HTML_FILE_NAME]
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

// TODO: read from vite config
const PUBLIC_FOLDER_PATH = 'public'

function isInRootPath(toImport: string, publicDir: string = PUBLIC_FOLDER_PATH): boolean {
  return toImport.startsWith('/') && !toImport.startsWith(`/${publicDir}/`)
}

function resolveModuleFromPublicDir(
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  importOrigin: string,
  toImport: string,
  publicDir: string = PUBLIC_FOLDER_PATH,
): FileLookupResult {
  const publicPath = `/${publicDir}/${toImport}`.replace('//', '/').replace('/./', '/')
  return resolveModule(projectContents, nodeModules, importOrigin, publicPath)
}
