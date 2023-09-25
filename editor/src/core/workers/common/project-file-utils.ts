import type {
  ImportDetails,
  Imports,
  ParsedJSONFailure,
  ParsedJSONSuccess,
  ImportAlias,
  RevisionsStateType,
} from '../../shared/project-file-types'
import { RevisionsState } from '../../shared/project-file-types'
import { fastForEach } from '../../shared/utils'
import { defaultIfNull, optionalMap } from '../../shared/optional-utils'

import { absolutePathFromRelativePath } from '../../../utils/path-utils'
import { stripExtension } from '../../../components/custom-code/custom-code-utils'

export function codeNeedsPrinting(revisionsState: RevisionsStateType): boolean {
  return revisionsState === RevisionsState.ParsedAhead
}

export function codeNeedsParsing(revisionsState: RevisionsStateType): boolean {
  return revisionsState === RevisionsState.CodeAhead
}

export function emptyImports(): Imports {
  return {}
}

function mergeImportDetails(first: ImportDetails, second: ImportDetails): ImportDetails {
  /**
   * import * as Name from 'import'
   * and
   * import Name from 'import'
   * are the same
   */

  if (
    first.importedWithName != null &&
    second.importedAs != null &&
    first.importedFromWithin.length === 0 &&
    second.importedFromWithin.length === 0
  ) {
    return {
      importedWithName: first.importedWithName,
      importedAs: first.importedAs,
      importedFromWithin: [],
    }
  }
  let importedFromWithin: Array<ImportAlias> = [...first.importedFromWithin]
  fastForEach(second.importedFromWithin, (secondWithin) => {
    if (
      importedFromWithin.find(
        (i) => i.name === secondWithin.name && i.alias === secondWithin.alias,
      ) == null
    ) {
      importedFromWithin.push(secondWithin)
    }
  })
  const importedWithName = defaultIfNull(second.importedWithName, first.importedWithName)
  const importedAs = defaultIfNull(second.importedAs, first.importedAs)
  return {
    importedWithName: importedWithName,
    importedFromWithin: importedFromWithin,
    importedAs: importedAs,
  }
}

function mergeMaybeImportDetails(
  left: ImportDetails | null,
  right: ImportDetails | null,
): ImportDetails | null {
  return (
    optionalMap((l) => optionalMap((r) => mergeImportDetails(l, r), right), left) ??
    left ??
    right ??
    null
  )
}

export function mergeImports(fileUri: string, first: Imports, second: Imports): Imports {
  const allImportSources = new Set([...Object.keys(first), ...Object.keys(second)])
  let absoluteImportSourcePathsToRelativeImportSourcePaths: {
    [absolutePath: string]: string | undefined
  } = {}
  let imports: Imports = {}

  allImportSources.forEach((importSource) => {
    const rawAbsolutePath = absolutePathFromRelativePath(fileUri, false, importSource)
    if (fileUri === rawAbsolutePath) {
      // Prevent accidentally importing the current file
      return
    }

    const absoluteImportSource = stripExtension(rawAbsolutePath)

    let existingImportSourceToUse: string | undefined =
      absoluteImportSourcePathsToRelativeImportSourcePaths[absoluteImportSource]

    if (existingImportSourceToUse == null) {
      absoluteImportSourcePathsToRelativeImportSourcePaths[absoluteImportSource] = importSource
      existingImportSourceToUse = importSource
    }

    const importDetailsFromFirst: ImportDetails | null = first[importSource] ?? null
    const importDetailsFromSecond: ImportDetails | null = second[importSource] ?? null
    const existingImport: ImportDetails | null = imports[existingImportSourceToUse] ?? null
    const merged: ImportDetails | null = mergeMaybeImportDetails(
      existingImport,
      mergeMaybeImportDetails(importDetailsFromFirst, importDetailsFromSecond),
    )

    if (merged != null) {
      imports[existingImportSourceToUse] = merged
    }
  })
  return imports
}

export function addImport(
  fileUri: string,
  importedFrom: string,
  importedWithName: string | null,
  importedFromWithin: Array<ImportAlias>,
  importedAs: string | null,
  imports: Imports,
): Imports {
  const toAdd: Imports = {
    [importedFrom]: {
      importedWithName: importedWithName,
      importedFromWithin: importedFromWithin,
      importedAs: importedAs,
    },
  }
  return mergeImports(fileUri, imports, toAdd)
}

export function parsedJSONSuccess(value: any): ParsedJSONSuccess {
  return {
    type: 'SUCCESS',
    value: value,
  }
}

export function parsedJSONFailure(
  codeSnippet: string,
  reason: string,
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
): ParsedJSONFailure {
  return {
    type: 'FAILURE',
    codeSnippet: codeSnippet,
    reason: reason,
    startLine: startLine,
    startCol: startCol,
    endLine: endLine,
    endCol: endCol,
  }
}
