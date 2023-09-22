import * as TS from 'typescript-for-the-editor'
import type {
  ImportDetails,
  Imports,
  ParsedJSONFailure,
  ParsedJSONSuccess,
  ImportAlias,
  RevisionsStateType,
} from '../../shared/project-file-types'
import {
  TextFile,
  ParseFailure,
  ParseSuccess,
  RevisionsState,
  HighlightBoundsForUids,
  isParseSuccess,
  ExportsDetail,
} from '../../shared/project-file-types'
import { fastForEach } from '../../shared/utils'
import { defaultIfNull } from '../../shared/optional-utils'
import { ErrorMessage } from '../../shared/error-messages'

import { ArbitraryJSBlock, Comment, TopLevelElement } from '../../shared/element-template'
import { emptySet } from '../../shared/set-utils'
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

export function mergeImports(fileUri: string, first: Imports, second: Imports): Imports {
  const allKeys = new Set([...Object.keys(first), ...Object.keys(second)])
  let absoluteKeysToRelativeKeys: { [absolutePath: string]: string } = {}
  let imports: Imports = {}
  allKeys.forEach((key) => {
    let existingKeyToUse = key
    const rawAbsolutePath = absolutePathFromRelativePath(fileUri, false, key)
    if (fileUri === rawAbsolutePath) {
      // Prevent accidentally importing the current file
      return
    }

    const absoluteKey = stripExtension(rawAbsolutePath)

    if (absoluteKeysToRelativeKeys[absoluteKey] == null) {
      absoluteKeysToRelativeKeys[absoluteKey] = key
    } else {
      existingKeyToUse = absoluteKeysToRelativeKeys[absoluteKey]
    }
    const firstValue = first[key]
    const secondValue = second[key]
    let mergedValues: ImportDetails | undefined
    if (firstValue === undefined) {
      if (secondValue === undefined) {
        mergedValues = undefined
      } else {
        mergedValues = secondValue
      }
    } else {
      if (secondValue === undefined) {
        mergedValues = firstValue
      } else {
        mergedValues = mergeImportDetails(firstValue, secondValue)
      }
    }
    // Merge the two values into whatever may already exist.
    const existingValue = imports[existingKeyToUse]
    if (mergedValues !== undefined) {
      if (existingValue === undefined) {
        imports[existingKeyToUse] = mergedValues
      } else {
        imports[existingKeyToUse] = mergeImportDetails(existingValue, mergedValues)
      }
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
