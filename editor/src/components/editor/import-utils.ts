import { resolveModulePathIncludingBuiltIns } from '../../core/es-modules/package-manager/module-resolution'
import { foldEither } from '../../core/shared/either'

import {
  applyFilePathMappingsToFilePath,
  emptyImports,
  emptyImportsMergeResolution,
  mergeImports,
} from '../../core/workers/common/project-file-utils'
import type {
  ImportInfo,
  JSXElement,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../core/shared/element-template'

import {
  importedOrigin,
  isIntrinsicElement,
  isJSXElement,
  isJSXFragment,
  sameFileOrigin,
  walkElement,
  walkElements,
} from '../../core/shared/element-template'
import type {
  ElementPath,
  ImportsMergeResolution,
  ImportDetails,
  Imports,
  NodeModules,
} from '../../core/shared/project-file-types'
import { importAlias, importDetails, importsResolution } from '../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from '../assets'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { withUnderlyingTarget } from './store/editor-state'
import * as EP from '../../core/shared/element-path'
import { renameDuplicateImportsInMergeResolution } from '../../core/shared/import-shared-utils'
import { getParseSuccessForFilePath } from '../canvas/canvas-utils'
import type { FilePathMappings } from '../../core/model/project-file-utils'
import { getFilePathMappings } from '../../core/model/project-file-utils'

export function getRequiredImportsForElement(
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  targetFilePath: string,
  builtInDependencies: BuiltInDependencies,
): ImportsMergeResolution {
  const filePathMappings = getFilePathMappings(projectContents)
  return withUnderlyingTarget<ImportsMergeResolution>(
    target,
    projectContents,
    emptyImportsMergeResolution(),
    (success, element, underlyingTarget, underlyingFilePath) => {
      const importsInOriginFile = success.imports
      const topLevelElementsInOriginFile = success.topLevelElements
      const lastPathPart =
        EP.lastElementPathForPath(underlyingTarget) ?? EP.emptyStaticElementPathPart()

      let importsMergeResolution = emptyImportsMergeResolution()

      // Walk down through the elements as elements within the element being reparented might also be imported.
      walkElement(element, lastPathPart, 0, (elem, subPath, depth) => {
        if (isJSXElement(elem)) {
          // Straight up ignore intrinsic elements as they wont be imported.
          if (!isIntrinsicElement(elem.name)) {
            const importedFromResult = importedFromWhere(
              filePathMappings,
              underlyingFilePath,
              elem.name.baseVariable,
              topLevelElementsInOriginFile,
              importsInOriginFile,
            )

            if (importedFromResult != null) {
              switch (importedFromResult.type) {
                case 'SAME_FILE_ORIGIN':
                  importsMergeResolution = mergeImportsResolutions(
                    targetFilePath,
                    filePathMappings,
                    importsMergeResolution,
                    importsResolution(
                      getImportsFor(
                        builtInDependencies,
                        importsInOriginFile,
                        projectContents,
                        nodeModules,
                        underlyingFilePath,
                        elem.name.baseVariable,
                      ),
                    ),
                  )
                  break
                case 'IMPORTED_ORIGIN':
                  if (importedFromResult.exportedName != null) {
                    importsMergeResolution = mergeImportsResolutions(
                      targetFilePath,
                      filePathMappings,
                      importsMergeResolution,
                      importsResolution(
                        getImportsFor(
                          builtInDependencies,
                          importsInOriginFile,
                          projectContents,
                          nodeModules,
                          underlyingFilePath,
                          importedFromResult.exportedName,
                          importedFromResult.variableName,
                        ),
                      ),
                    )
                  }
                  break
                default:
                  const _exhaustiveCheck: never = importedFromResult
                  throw new Error(
                    `Unhandled imported from result ${JSON.stringify(importedFromResult)}`,
                  )
              }
            }
          }
        } else if (isJSXFragment(elem) && elem.longForm) {
          importsMergeResolution = mergeImportsResolutions(
            targetFilePath,
            filePathMappings,
            importsMergeResolution,
            importsResolution({
              react: {
                importedAs: 'React',
                importedFromWithin: [],
                importedWithName: null,
              },
            }),
          )
        }
      })

      // adjust imports in case of duplicate names
      const targetFileContents = getParseSuccessForFilePath(targetFilePath, projectContents)
      const duplicateImportsResolution = renameDuplicateImportsInMergeResolution(
        targetFileContents.imports,
        importsMergeResolution,
        targetFilePath,
        filePathMappings,
      )

      return duplicateImportsResolution
    },
  )
}

type ImportedFromWhereResult = ImportInfo

export function importedFromWhere(
  filePathMappings: FilePathMappings,
  originFilePath: string,
  variableName: string,
  topLevelElements: Array<TopLevelElement>,
  importsToSearch: Imports,
): ImportedFromWhereResult | null {
  for (const topLevelElement of topLevelElements) {
    switch (topLevelElement.type) {
      case 'UTOPIA_JSX_COMPONENT':
        if (topLevelElement.name === variableName) {
          return sameFileOrigin(originFilePath, variableName)
        }
        break
      case 'ARBITRARY_JS_BLOCK':
        if (topLevelElement.definedWithin.includes(variableName)) {
          return sameFileOrigin(originFilePath, variableName)
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
    const importSourceMapped = applyFilePathMappingsToFilePath(importSource, filePathMappings)
    if (specificImport.importedAs === variableName) {
      return importedOrigin(importSourceMapped, variableName, null)
    }
    if (specificImport.importedWithName === variableName) {
      return importedOrigin(importSourceMapped, variableName, null)
    }
    for (const fromWithin of specificImport.importedFromWithin) {
      if (fromWithin.alias === variableName) {
        return importedOrigin(importSourceMapped, variableName, fromWithin.name)
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
  builtInDependencies: BuiltInDependencies,
  currentImports: Imports,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  importOrigin: string,
  importedName: string,
  importedAlias?: string,
): Imports {
  for (const fileKey of Object.keys(currentImports)) {
    const details = currentImports[fileKey]
    const importPath = resolveModulePathIncludingBuiltIns(
      builtInDependencies,
      projectContents,
      nodeModules,
      importOrigin,
      fileKey,
    )
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
      if (importedAlias == null && fromWithin.alias === importedName) {
        return {
          [resolvedImportPath]: importDetails(
            null,
            [importAlias(importedName, importedName)],
            null,
          ),
        }
      } else if (fromWithin.alias === importedAlias) {
        return {
          [resolvedImportPath]: importDetails(
            null,
            [importAlias(importedName, importedAlias)],
            null,
          ),
        }
      }
    }
  }

  return emptyImports()
}

export function mergeImportsResolutions(
  fileUri: string,
  filePathMappings: FilePathMappings,
  existing: ImportsMergeResolution,
  newImports: ImportsMergeResolution,
): ImportsMergeResolution {
  const { imports, duplicateNameMapping } = mergeImports(
    fileUri,
    filePathMappings,
    existing.imports,
    newImports.imports,
  )
  const mergedDuplicateNameMapping = new Map<string, string>([
    ...existing.duplicateNameMapping,
    ...newImports.duplicateNameMapping,
    ...duplicateNameMapping,
  ])
  return {
    imports: imports,
    duplicateNameMapping: mergedDuplicateNameMapping,
  }
}

export function removeUnusedImportsForRemovedElement(
  removedElement: JSXElement,
  remainingComponents: UtopiaJSXComponent[],
  imports: Imports,
): Imports {
  const elementName = removedElement.name.baseVariable
  const remainingComponentNames = new Set<string>()
  walkElements(remainingComponents, (jsxElement) => {
    if (isJSXElement(jsxElement)) {
      remainingComponentNames.add(jsxElement.name.baseVariable)
    }
  })
  // if some other element is using this import, don't remove it
  if (remainingComponentNames.has(elementName)) {
    return imports
  }
  // remove the import
  return removeImports(imports, [elementName])
}

export function removeImports(imports: Imports, namesToRemove: string[]): Imports {
  let newImports: Imports = {}
  for (const importPath of Object.keys(imports)) {
    const newImportDetails = removeImportDetails(imports[importPath], namesToRemove)
    if (newImportDetails != null) {
      newImports[importPath] = newImportDetails
    }
  }
  return newImports
}

export function purgeEmptyImports(imports: Imports): Imports {
  let newImports: Imports = {}
  for (const importPath of Object.keys(imports)) {
    const details = imports[importPath]
    if (
      details.importedWithName != null ||
      details.importedAs != null ||
      details.importedFromWithin.length > 0
    ) {
      newImports[importPath] = details
    }
  }
  return newImports
}

function removeImportDetails(
  details: ImportDetails,
  namesToRemove: string[],
): ImportDetails | null {
  const importedFromWithin = details.importedFromWithin.filter(
    (fromWithin) => !namesToRemove.includes(fromWithin.alias),
  )
  const importedWithName =
    details.importedWithName == null || namesToRemove.includes(details.importedWithName)
      ? null
      : details.importedWithName
  const importedAs =
    details.importedAs == null || namesToRemove.includes(details.importedAs)
      ? null
      : details.importedAs

  if (importedWithName == null && importedAs == null && importedFromWithin.length === 0) {
    return null
  }

  return {
    importedWithName: importedWithName,
    importedAs: importedAs,
    importedFromWithin: importedFromWithin,
  }
}
