import { resolveModulePathIncludingBuiltIns } from '../../core/es-modules/package-manager/module-resolution'
import { foldEither } from '../../core/shared/either'
import { emptyImports, mergeImports } from '../../core/workers/common/project-file-utils'
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
  ImportDetails,
  Imports,
  NodeModules,
} from '../../core/shared/project-file-types'
import { importAlias, importDetails } from '../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from '../assets'
import type { BuiltInDependencies } from '../../core/es-modules/package-manager/built-in-dependencies-list'
import { withUnderlyingTarget } from './store/editor-state'
import * as EP from '../../core/shared/element-path'

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
