import type { Imports, ImportsMergeResolution } from './project-file-types'
import { mapValues } from './object-utils'
import { importAlias, importDetails } from './project-file-types'
import { absolutePathFromRelativePath } from '../../utils/path-utils'
import { stripExtension } from '../../components/custom-code/custom-code-utils'
import type { FilePathMappings } from '../model/project-file-utils'
import { applyFilePathMappingsToFilePath } from '../workers/common/project-file-utils'

export function renameDuplicateImports(
  existingImports: Imports,
  toAdd: Imports,
  targetFilePath: string,
  filePathMappings: FilePathMappings,
): ImportsMergeResolution {
  function absolutePath(relativePath: string): string {
    const rawAbsolutePath = absolutePathFromRelativePath(targetFilePath, false, relativePath)
    const afterFilePathMapping = applyFilePathMappingsToFilePath(rawAbsolutePath, filePathMappings)
    const absoluteImportSource = stripExtension(afterFilePathMapping)
    return absoluteImportSource
  }
  const existingNames = getAllImportsUniqueNames(existingImports)
  const duplicateNameMapping = new Map<string, string>()

  const importResult = mapValues((original, importSource) => {
    let importedWithName = original.importedWithName,
      importedAs = original.importedAs

    // importedWithName
    if (original.importedWithName != null) {
      importedWithName = adjustImportNameIfNeeded(
        existingNames,
        original.importedWithName,
        importSource,
        'importedWithName',
        absolutePath,
      )
      if (importedWithName !== original.importedWithName) {
        duplicateNameMapping.set(original.importedWithName, importedWithName)
      }
      existingNames.set(importedWithName, { source: importSource, type: 'importedWithName' })
    }

    // importedAs
    if (original.importedAs != null) {
      importedAs = adjustImportNameIfNeeded(
        existingNames,
        original.importedAs,
        importSource,
        'importedAs',
        absolutePath,
      )
      if (importedAs !== original.importedAs) {
        duplicateNameMapping.set(original.importedAs, importedAs)
      }
      existingNames.set(importedAs, { source: importSource, type: 'importedAs' })
    }

    // importedFromWithin
    const importedFromWithin = original.importedFromWithin.map((importAliasDetails) => {
      let alias = importAliasDetails.alias
      let originalName = importAliasDetails.name
      alias = adjustImportNameIfNeeded(
        existingNames,
        importAliasDetails.alias,
        importSource,
        'importedFromWithin',
        absolutePath,
        originalName,
      )
      if (alias !== importAliasDetails.alias) {
        duplicateNameMapping.set(importAliasDetails.alias, alias)
      }
      existingNames.set(alias, {
        source: importSource,
        type: 'importedFromWithin',
        originalName: importAliasDetails.name,
      })
      return importAlias(importAliasDetails.name, alias)
    })

    return importDetails(importedWithName, importedFromWithin, importedAs)
  }, toAdd)

  return {
    imports: importResult,
    duplicateNameMapping: duplicateNameMapping,
  }
}

export function renameDuplicateImportsInMergeResolution(
  existingImports: Imports,
  toAdd: ImportsMergeResolution,
  targetFilePath: string,
  filePathMappings: FilePathMappings,
): ImportsMergeResolution {
  const importsResolution = renameDuplicateImports(
    existingImports,
    toAdd.imports,
    targetFilePath,
    filePathMappings,
  )
  const mergedDuplicateNameMapping = mergeDuplicateNameMaps(importsResolution, toAdd)
  return {
    imports: importsResolution.imports,
    duplicateNameMapping: mergedDuplicateNameMapping,
  }
}

function adjustImportNameIfNeeded(
  existingNames: ImportUniqueNames,
  importName: string,
  importSource: string,
  type: ImportType,
  absolutePath: (relativePath: string) => string,
  originalName?: string,
): string {
  const existingImport = existingNames.get(importName)
  if (existingImport != null) {
    /** first - check to see if the new import is already in the existing imports, renamed
     * import { Card } from './fileA'
     * import { Card as Card_2 } from './fileB'
     * ++ import { Card } from './fileB'
     * turns to:
     * import { Card } from './fileA'
     * import { Card as Card_2 } from './fileB'
     * */
    const existingImportAlias = findOriginalNameInExistingImports(
      importName,
      importSource,
      type,
      existingNames,
      absolutePath,
      originalName,
    )
    if (existingImportAlias != null) {
      return existingImportAlias
    }
    /** different source - we always rename the new import
     * import { Card } from './fileA'
     * ++ import { Card } from './fileB'
     * turns to:
     * import { Card } from './fileA'
     * ++ import { Card as Card_2 } from './fileB'
     * */
    if (absolutePath(existingImport.source) !== absolutePath(importSource)) {
      return findNewImportName(importName, existingNames)
    }
    /** same source with a different type - we always rename the new import -
     * unless it's a default and named import (to maintain existing behavior)
     * import Card from './fileA'
     * ++ import { Card } from './fileA'
     * turns to:
     * import Card from './fileA'
     * ++ import { Card as Card_2 } from './fileA'
     * */
    if (
      existingImport.type !== type &&
      [existingImport.type, type].includes('importedFromWithin')
    ) {
      return findNewImportName(importName, existingNames)
    }
    /** edge case - same source, same alias, different originalName
     * import { Card as Card_2 } from './fileA'
     * ++ import { Card_New as Card_2 } from './fileA'
     * turns to:
     * import { Card as Card_2 } from './fileA'
     * ++ import { Card_New as Card_3 } from './fileA'
     * */
    if (
      existingImport.type === 'importedFromWithin' &&
      type === 'importedFromWithin' &&
      existingImport.originalName !== importName
    ) {
      return findNewImportName(importName, existingNames)
    }
  }
  return importName
}

function findNewImportName(currentName: string, existingNames: ImportUniqueNames) {
  let newName = currentName
  let i = 2
  while (existingNames.has(newName)) {
    newName = `${currentName}_${i}`
    i++
  }
  return newName
}

function findOriginalNameInExistingImports(
  currentName: string,
  importSource: string,
  importType: ImportType,
  existingNames: ImportUniqueNames,
  absolutePath: (relativePath: string) => string,
  originalName?: string,
): string | null {
  let existingImportAlias: string | null = null
  // check if the new import is already in the existing imports, renamed
  existingNames.forEach((existingImportData, existingName) => {
    if (absolutePath(existingImportData.source) === absolutePath(importSource)) {
      if (existingImportData.type === importType) {
        if (
          importType !== 'importedFromWithin' ||
          existingImportData.originalName === currentName ||
          existingImportData.originalName === originalName
        ) {
          existingImportAlias = existingName
        }
      }
    }
  })
  return existingImportAlias
}

export function getAllImportsUniqueNames(imports: Imports): ImportUniqueNames {
  return Object.entries(imports).reduce((acc, [importSource, details]) => {
    if (details.importedAs !== null) {
      acc.set(details.importedAs, { source: importSource, type: 'importedAs' })
    }
    if (details.importedWithName !== null) {
      acc.set(details.importedWithName, { source: importSource, type: 'importedWithName' })
    }
    details.importedFromWithin.forEach((importedFromWithin) => {
      acc.set(importedFromWithin.alias, {
        source: importSource,
        type: 'importedFromWithin',
        originalName: importedFromWithin.name,
      })
    })
    return acc
  }, new Map())
}

export function mergeDuplicateNameMaps(
  first: ImportsMergeResolution,
  second: ImportsMergeResolution,
): Map<string, string> {
  const mergedDuplicateNameMapping = new Map<string, string>([
    ...first.duplicateNameMapping,
    ...second.duplicateNameMapping,
  ])
  return mergedDuplicateNameMapping
}

type ImportType = 'importedAs' | 'importedWithName' | 'importedFromWithin'
type ImportUniqueNames = Map<string, { source: string; type: ImportType; originalName?: string }>
