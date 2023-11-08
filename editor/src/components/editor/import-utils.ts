import { resolveModulePathIncludingBuiltIns } from '../../core/es-modules/package-manager/module-resolution'
import { foldEither } from '../../core/shared/either'
import { emptyImports, mergeImports } from '../../core/workers/common/project-file-utils'
import type { ImportInfo, TopLevelElement } from '../../core/shared/element-template'
import {
  importedOrigin,
  isIntrinsicElement,
  isJSXElement,
  isJSXFragment,
  sameFileOrigin,
  walkElement,
} from '../../core/shared/element-template'
import type {
  DuplicateImportsResolution,
  ElementPath,
  Imports,
  NodeModules,
} from '../../core/shared/project-file-types'
import { importAlias, importDetails } from '../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from '../assets'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { withUnderlyingTarget } from './store/editor-state'
import * as EP from '../../core/shared/element-path'
import { mapValues } from '../../core/shared/object-utils'

export function getRequiredImportsForElement(
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  targetFilePath: string,
  builtInDependencies: BuiltInDependencies,
): Imports {
  return withUnderlyingTarget<Imports>(
    target,
    projectContents,
    emptyImports(),
    (success, element, underlyingTarget, underlyingFilePath) => {
      const importsInOriginFile = success.imports
      const topLevelElementsInOriginFile = success.topLevelElements
      const lastPathPart =
        EP.lastElementPathForPath(underlyingTarget) ?? EP.emptyStaticElementPathPart()

      let importsToAdd: Imports = emptyImports()
      // Walk down through the elements as elements within the element being reparented might also be imported.
      walkElement(element, lastPathPart, 0, (elem, subPath, depth) => {
        if (isJSXElement(elem)) {
          // Straight up ignore intrinsic elements as they wont be imported.
          if (!isIntrinsicElement(elem.name)) {
            const importedFromResult = importedFromWhere(
              underlyingFilePath,
              elem.name.baseVariable,
              topLevelElementsInOriginFile,
              importsInOriginFile,
            )

            if (importedFromResult != null) {
              switch (importedFromResult.type) {
                case 'SAME_FILE_ORIGIN':
                  importsToAdd = mergeImports(
                    targetFilePath,
                    importsToAdd,
                    getImportsFor(
                      builtInDependencies,
                      importsInOriginFile,
                      projectContents,
                      nodeModules,
                      underlyingFilePath,
                      elem.name.baseVariable,
                    ),
                  )
                  break
                case 'IMPORTED_ORIGIN':
                  if (importedFromResult.exportedName != null) {
                    importsToAdd = mergeImports(
                      targetFilePath,
                      importsToAdd,
                      getImportsFor(
                        builtInDependencies,
                        importsInOriginFile,
                        projectContents,
                        nodeModules,
                        underlyingFilePath,
                        importedFromResult.exportedName,
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
          importsToAdd = mergeImports(targetFilePath, importsToAdd, {
            react: {
              importedAs: 'React',
              importedFromWithin: [],
              importedWithName: null,
            },
          })
        }
      })

      return importsToAdd
    },
  )
}

type ImportedFromWhereResult = ImportInfo

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
    if (specificImport.importedAs === variableName) {
      return importedOrigin(importSource, variableName, null)
    }
    if (specificImport.importedWithName === variableName) {
      return importedOrigin(importSource, variableName, null)
    }
    for (const fromWithin of specificImport.importedFromWithin) {
      if (fromWithin.alias === variableName) {
        return importedOrigin(importSource, variableName, fromWithin.name)
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
export function getAllImportsUniqueNames(imports: Imports): Map<string, string> {
  return Object.entries(imports).reduce((acc, [importSource, details]) => {
    if (details.importedAs !== null) {
      acc.set(details.importedAs, importSource)
    }
    if (details.importedWithName !== null) {
      acc.set(details.importedWithName, importSource)
    }
    details.importedFromWithin.forEach((importedFromWithin) => {
      acc.set(importedFromWithin.alias, importSource)
    })
    return acc
  }, new Map<string, string>())
}

export function handleDuplicateImports(
  existingImports: Imports,
  toAdd: Imports,
): DuplicateImportsResolution {
  const existingNames = getAllImportsUniqueNames(existingImports)
  const nameMapping = new Map<string, string>()

  const importResult = mapValues((original, importSource) => {
    let importedWithName = original.importedWithName,
      importedAs = original.importedAs

    // importedWithName
    if (original.importedWithName != null) {
      importedWithName = adjustImportNameIfNeeded(
        existingNames,
        original.importedWithName,
        importSource,
      )
      if (importedWithName !== original.importedWithName) {
        nameMapping.set(original.importedWithName, importedWithName)
        existingNames.set(importedWithName, importSource)
      }
    }

    // importedAs
    if (original.importedAs != null) {
      importedAs = adjustImportNameIfNeeded(existingNames, original.importedAs, importSource)
      if (importedAs !== original.importedAs) {
        nameMapping.set(original.importedAs, importedAs)
        existingNames.set(importedAs, importSource)
      }
    }

    // importedFromWithin
    const importedFromWithin = original.importedFromWithin.map((importAliasDetails) => {
      let alias = importAliasDetails.alias
      alias = adjustImportNameIfNeeded(existingNames, importAliasDetails.alias, importSource)
      if (alias !== importAliasDetails.alias) {
        nameMapping.set(importAliasDetails.alias, alias)
        existingNames.set(alias, importSource)
      }
      return importAlias(importAliasDetails.name, alias)
    })

    return importDetails(importedWithName, importedFromWithin, importedAs)
  }, toAdd)

  return {
    imports: importResult,
    nameMapping: nameMapping,
  }
}

function adjustImportNameIfNeeded(
  existingNames: Map<string, string>,
  importName: string,
  importSource: string,
): string {
  if (existingNames.has(importName) && existingNames.get(importName) !== importSource) {
    const newName = `${importName}_${pathHash(importSource)}`
    return newName
  }
  return importName
}

function pathHash(path: string) {
  return Buffer.from(path).toString('base64').slice(0, 4)
}
