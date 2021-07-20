import * as TS from 'typescript'
import {
  TextFile,
  ParseFailure,
  ParseSuccess,
  RevisionsState,
  ImportDetails,
  Imports,
  HighlightBoundsForUids,
  ParsedJSONFailure,
  ParsedJSONSuccess,
  ImportAlias,
  isParseSuccess,
  ExportsDetail,
} from '../../shared/project-file-types'
import { fastForEach } from '../../shared/utils'
import { defaultIfNull } from '../../shared/optional-utils'
import { ErrorMessage } from '../../shared/error-messages'

import { printCode, printCodeOptions } from '../parser-printer/parser-printer'
import { ArbitraryJSBlock, Comment, TopLevelElement } from '../../shared/element-template'
import { emptySet } from '../../shared/set-utils'
import { absolutePathFromRelativePath } from '../../../utils/path-utils'

export function codeNeedsPrinting(revisionsState: RevisionsState): boolean {
  return revisionsState === RevisionsState.ParsedAhead
}

export function codeNeedsParsing(revisionsState: RevisionsState): boolean {
  return revisionsState === RevisionsState.CodeAhead
}

export function getTextFileContents(
  file: TextFile,
  pretty: boolean,
  allowPrinting: boolean,
): string {
  if (
    allowPrinting &&
    codeNeedsPrinting(file.fileContents.revisionsState) &&
    isParseSuccess(file.fileContents.parsed)
  ) {
    return printCode(
      printCodeOptions(false, pretty, true),
      file.fileContents.parsed.imports,
      file.fileContents.parsed.topLevelElements,
      file.fileContents.parsed.jsxFactoryFunction,
      file.fileContents.parsed.exportsDetail,
    )
  } else {
    return file.fileContents.code
  }
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
    const absoluteKey = absolutePathFromRelativePath(fileUri, false, key)
    if (absoluteKeysToRelativeKeys[absoluteKey] != null) {
      existingKeyToUse = absoluteKeysToRelativeKeys[absoluteKey]
    } else {
      absoluteKeysToRelativeKeys[absoluteKey] = key
    }
    const firstValue = first[existingKeyToUse]
    const secondValue = second[key]
    if (firstValue === undefined) {
      if (secondValue !== undefined) {
        imports[existingKeyToUse] = secondValue
      }
    } else {
      if (secondValue === undefined) {
        imports[existingKeyToUse] = firstValue
      } else {
        imports[existingKeyToUse] = mergeImportDetails(firstValue, secondValue)
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

export function parseSuccess(
  imports: Imports,
  topLevelElements: Array<TopLevelElement>,
  highlightBounds: HighlightBoundsForUids,
  jsxFactoryFunction: string | null,
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null,
  exportsDetail: ExportsDetail,
): ParseSuccess {
  return {
    type: 'PARSE_SUCCESS',
    imports: imports,
    topLevelElements: topLevelElements,
    highlightBounds: highlightBounds,
    jsxFactoryFunction: jsxFactoryFunction,
    combinedTopLevelArbitraryBlock: combinedTopLevelArbitraryBlock,
    exportsDetail: exportsDetail,
  }
}

export function parseFailure(
  diagnostics: Array<TS.Diagnostic> | null,
  parsedJSON: ParsedJSONFailure | null,
  errorMessage: string | null,
  errorMessages: Array<ErrorMessage>,
): ParseFailure {
  return {
    type: 'PARSE_FAILURE',
    diagnostics: diagnostics,
    parsedJSONFailure: parsedJSON,
    errorMessage: errorMessage,
    errorMessages: errorMessages,
  }
}

export function parsedJSONSuccess(value: any): ParsedJSONSuccess {
  return {
    type: 'SUCCESS',
    value: value,
  }
}

export function parsedJSONFailure(
  errorNode: TS.Node,
  codeSnippet: string,
  reason: string,
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
): ParsedJSONFailure {
  return {
    type: 'FAILURE',
    errorNode: errorNode,
    codeSnippet: codeSnippet,
    reason: reason,
    startLine: startLine,
    startCol: startCol,
    endLine: endLine,
    endCol: endCol,
  }
}
