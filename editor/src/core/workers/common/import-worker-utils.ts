import type { Imports, ImportsMergeResolution } from '../../../core/shared/project-file-types'
import { mapValues } from '../../../core/shared/object-utils'
import { importAlias, importDetails } from '../../../core/shared/project-file-types'

export function handleDuplicateImports(
  existingImports: Imports,
  toAdd: Imports,
): ImportsMergeResolution {
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
      )
      if (importedWithName !== original.importedWithName) {
        duplicateNameMapping.set(original.importedWithName, importedWithName)
        existingNames.set(importedWithName, importSource)
      }
    }

    // importedAs
    if (original.importedAs != null) {
      importedAs = adjustImportNameIfNeeded(existingNames, original.importedAs, importSource)
      if (importedAs !== original.importedAs) {
        duplicateNameMapping.set(original.importedAs, importedAs)
        existingNames.set(importedAs, importSource)
      }
    }

    // importedFromWithin
    const importedFromWithin = original.importedFromWithin.map((importAliasDetails) => {
      let alias = importAliasDetails.alias
      alias = adjustImportNameIfNeeded(existingNames, importAliasDetails.alias, importSource)
      if (alias !== importAliasDetails.alias) {
        duplicateNameMapping.set(importAliasDetails.alias, alias)
        existingNames.set(alias, importSource)
      }
      return importAlias(importAliasDetails.name, alias)
    })

    return importDetails(importedWithName, importedFromWithin, importedAs)
  }, toAdd)

  return {
    imports: importResult,
    duplicateNameMapping: duplicateNameMapping,
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
