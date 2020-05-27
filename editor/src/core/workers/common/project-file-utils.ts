import * as TS from 'typescript'
import {
  ProjectFile,
  UIJSFile,
  CodeFile,
  ParseFailure,
  ParseSuccess,
  RevisionsState,
  ImportDetails,
  Imports,
  CanvasMetadata,
  PrintedCanvasMetadata,
  CanvasMetadataParseResult,
  HighlightBoundsForUids,
  ParsedJSONFailure,
  ParsedJSONSuccess,
  ImportAlias,
  HighlightBounds,
} from '../../shared/project-file-types'
import { foldEither } from '../../shared/either'
import { fastForEach, arrayContains } from '../../shared/utils'
import { defaultIfNull } from '../../shared/optional-utils'
import { ErrorMessage } from '../../shared/error-messages'

import { printCode, printCodeOptions } from '../parser-printer/parser-printer'
import { TopLevelElement } from '../../shared/element-template'
import { convertScenesToUtopiaCanvasComponent } from '../../model/scene-utils'

export function codeNeedsPrinting(revisionsState: RevisionsState): boolean {
  return revisionsState === RevisionsState.ParsedAhead
}

export function codeNeedsParsing(revisionsState: RevisionsState): boolean {
  return revisionsState === RevisionsState.CodeAhead
}

export function getCodeFileContents(
  file: CodeFile | UIJSFile,
  pretty: boolean,
  allowPrinting: boolean,
): string {
  switch (file.type) {
    case 'CODE_FILE':
      return file.fileContents
    case 'UI_JS_FILE':
      return foldEither(
        (failure: ParseFailure) => {
          return failure.code
        },
        (success: ParseSuccess) => {
          if (allowPrinting && (success.code == null || codeNeedsPrinting(file.revisionsState))) {
            return printCode(
              printCodeOptions(false, pretty, true),
              success.imports,
              success.topLevelElements,
              success.jsxFactoryFunction,
            )
          } else {
            return success.code
          }
        },
        file.fileContents,
      )
    default:
      const _exhaustiveCheck: never = file
      throw new Error(`Unhandled file type ${JSON.stringify(file)}`)
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
  importedFromWithin.sort((i1, i2) => {
    if (i1.name < i2.name) {
      return -1
    }
    if (i1.name > i2.name) {
      return 1
    }
    return 0
  })
  const importedWithName = defaultIfNull(second.importedWithName, first.importedWithName)
  const importedAs = defaultIfNull(second.importedAs, first.importedAs)
  return {
    importedWithName: importedWithName,
    importedFromWithin: importedFromWithin,
    importedAs: importedAs,
  }
}

export function mergeImports(first: Imports, second: Imports): Imports {
  let combinedKeys: Set<string> = new Set()
  fastForEach(Object.keys(first), (f) => combinedKeys.add(f))
  fastForEach(Object.keys(second), (s) => combinedKeys.add(s))
  let orderedKeys: Array<string> = Array.from(combinedKeys)
  orderedKeys.sort()
  let imports: Imports = {}
  orderedKeys.forEach((key) => {
    const firstValue = first[key]
    const secondValue = second[key]
    if (firstValue === undefined) {
      if (secondValue !== undefined) {
        imports[key] = secondValue
      }
    } else {
      if (secondValue === undefined) {
        imports[key] = firstValue
      } else {
        imports[key] = mergeImportDetails(firstValue, secondValue)
      }
    }
  })
  return imports
}

export function addImport(
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
  return mergeImports(imports, toAdd)
}

export function defaultCanvasMetadata(): CanvasMetadata {
  return {}
}

export function defaultPrintedCanvasMetadata(): PrintedCanvasMetadata {
  return {
    scenes: null,
    elementMetadata: {},
  }
}

export function parseSuccess(
  imports: Imports,
  topLevelElements: Array<TopLevelElement>,
  canvasMetadata: CanvasMetadataParseResult,
  projectContainedOldSceneMetadata: boolean,
  code: string,
  highlightBounds: HighlightBoundsForUids,
  dependencyOrdering: Array<string>,
  jsxFactoryFunction: string | null,
): ParseSuccess {
  return {
    imports: imports,
    topLevelElements: topLevelElements,
    canvasMetadata: canvasMetadata,
    projectContainedOldSceneMetadata: projectContainedOldSceneMetadata,
    code: code,
    highlightBounds: highlightBounds,
    dependencyOrdering: dependencyOrdering,
    jsxFactoryFunction: jsxFactoryFunction,
  }
}

export function parseFailure(
  diagnostics: Array<TS.Diagnostic> | null,
  parsedJSON: ParsedJSONFailure | null,
  errorMessage: string | null,
  errorMessages: Array<ErrorMessage>,
  code: string,
): ParseFailure {
  return {
    diagnostics: diagnostics,
    parsedJSONFailure: parsedJSON,
    errorMessage: errorMessage,
    errorMessages: errorMessages,
    code: code,
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
