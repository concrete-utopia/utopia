import type { ProjectContentTreeRoot } from '../../components/assets'
import { walkContentsTreeForParseSuccess } from '../../components/assets'
import {
  getDefinedElsewhereFromElementChild,
  type ArbitraryJSBlock,
  type JSXAttributes,
  type JSXElementChild,
  type TopLevelElement,
} from '../shared/element-template'
import { emptySet } from '../shared/set-utils'
import { assertNever, fastForEach } from '../shared/utils'
import { memoize } from '../shared/memoize'

export type DuplicateUIDsResult = { [key: string]: Array<Array<string>> }

type SecretTypeToDeliberatelyBreakCodeForRefactoring_DELETE_ME = { cica: { [uid: string]: string } }

interface GetAllUniqueUIDsResult {
  duplicateIDs: DuplicateUIDsResult
  filePathToUids: SecretTypeToDeliberatelyBreakCodeForRefactoring_DELETE_ME
}

export function getAllUniqueUidsFromLookup(
  lookup: SecretTypeToDeliberatelyBreakCodeForRefactoring_DELETE_ME,
): Array<string> {
  return Object.keys(lookup)
}

export function getFilePathForUid(
  mapping: SecretTypeToDeliberatelyBreakCodeForRefactoring_DELETE_ME,
  uid: string,
): string {
  return mapping.cica[uid]
}

interface GetAllUniqueUIDsWorkingResult {
  uniqueIDs: { [key: string]: Array<string> }
  duplicateIDs: { [key: string]: Array<Array<string>> }
  allIDs: Set<string>
  uidsToFilePaths: SecretTypeToDeliberatelyBreakCodeForRefactoring_DELETE_ME
}

export function emptyGetAllUniqueUIDsWorkingResult(): GetAllUniqueUIDsWorkingResult {
  return {
    uniqueIDs: {},
    duplicateIDs: {},
    allIDs: emptySet(),
    uidsToFilePaths: { cica: {} },
  }
}

export function getAllUniqueUIDsResultFromWorkingResult(
  workingResult: GetAllUniqueUIDsWorkingResult,
): GetAllUniqueUIDsResult {
  return {
    duplicateIDs: workingResult.duplicateIDs,
    filePathToUids: workingResult.uidsToFilePaths,
  }
}

function checkUID(
  workingResult: GetAllUniqueUIDsWorkingResult,
  filePath: string,
  debugPath: Array<string>,
  uid: string,
  value: any,
): void {
  workingResult.allIDs.add(uid)
  workingResult.uidsToFilePaths.cica[uid] = filePath
  if (uid in workingResult.duplicateIDs) {
    workingResult.duplicateIDs[uid].push(debugPath)
  } else {
    if (uid in workingResult.uniqueIDs) {
      const currentUniqueIDsEntry = workingResult.uniqueIDs[uid]
      workingResult.duplicateIDs[uid] = [currentUniqueIDsEntry, debugPath]
      delete workingResult.uniqueIDs[uid]
    } else {
      workingResult.uniqueIDs[uid] = debugPath
    }
  }
}

function extractUidFromAttributes(
  workingResult: GetAllUniqueUIDsWorkingResult,
  filePath: string,
  debugPath: Array<string>,
  attributes: JSXAttributes,
): void {
  for (const attributePart of attributes) {
    switch (attributePart.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        extractUidFromJSXElementChild(workingResult, filePath, debugPath, attributePart.value)
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        extractUidFromJSXElementChild(workingResult, filePath, debugPath, attributePart.spreadValue)
        break
      default:
        assertNever(attributePart)
    }
  }
}

function extractUidFromJSXElementChild(
  workingResult: GetAllUniqueUIDsWorkingResult,
  filePath: string,
  debugPath: Array<string>,
  element: JSXElementChild,
): void {
  const newDebugPath = [...debugPath, element.uid]
  checkUID(workingResult, filePath, newDebugPath, element.uid, element)
  switch (element.type) {
    case 'JSX_ELEMENT':
      fastForEach(element.children, (child) =>
        extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, child),
      )
      extractUidFromAttributes(workingResult, filePath, newDebugPath, element.props)
      break
    case 'JSX_FRAGMENT':
      fastForEach(element.children, (child) =>
        extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, child),
      )
      break
    case 'JSX_CONDITIONAL_EXPRESSION':
      extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, element.condition)
      extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, element.whenTrue)
      extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, element.whenFalse)
      break
    case 'JSX_TEXT_BLOCK':
      break
    case 'ATTRIBUTE_VALUE':
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      for (const contentPart of element.content) {
        extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, contentPart.value)
      }
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      for (const contentPart of element.content) {
        extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, contentPart.value)
      }
      break
    case 'JSX_MAP_EXPRESSION':
      extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, element.valueToMap)
      extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, element.mapFunction)
      break
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      for (const elementWithin of Object.values(element.elementsWithin)) {
        extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, elementWithin)
      }
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      for (const parameter of element.parameters) {
        extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, parameter)
      }
      break
    case 'JS_IDENTIFIER':
      break
    case 'JS_PROPERTY_ACCESS':
      extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, element.onValue)
      break
    case 'JS_ELEMENT_ACCESS':
      extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, element.onValue)
      extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, element.element)
      break
    default:
      assertNever(element)
  }
}

function extractUIDFromArbitraryBlock(
  workingResult: GetAllUniqueUIDsWorkingResult,
  filePath: string,
  debugPath: Array<string>,
  arbitraryBlock: ArbitraryJSBlock,
): void {
  const newDebugPath = [...debugPath, arbitraryBlock.uid]
  checkUID(workingResult, filePath, newDebugPath, arbitraryBlock.uid, arbitraryBlock)
  for (const elementWithin of Object.values(arbitraryBlock.elementsWithin)) {
    extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, elementWithin)
  }
}

export function extractUIDFromTopLevelElement(
  workingResult: GetAllUniqueUIDsWorkingResult,
  filePath: string,
  debugPath: Array<string>,
  topLevelElement: TopLevelElement,
): void {
  switch (topLevelElement.type) {
    case 'UTOPIA_JSX_COMPONENT':
      extractUidFromJSXElementChild(workingResult, filePath, debugPath, topLevelElement.rootElement)
      if (topLevelElement.arbitraryJSBlock != null) {
        extractUIDFromArbitraryBlock(
          workingResult,
          filePath,
          debugPath,
          topLevelElement.arbitraryJSBlock,
        )
      }
      break
    case 'ARBITRARY_JS_BLOCK':
      extractUIDFromArbitraryBlock(workingResult, filePath, debugPath, topLevelElement)
      break
    case 'IMPORT_STATEMENT':
      break
    case 'UNPARSED_CODE':
      break
    default:
      assertNever(topLevelElement)
  }
}

export function getAllUniqueUidsInnerOld(
  projectContents: ProjectContentTreeRoot,
): GetAllUniqueUIDsResult {
  const workingResult = emptyGetAllUniqueUIDsWorkingResult()

  walkContentsTreeForParseSuccess(projectContents, (filePath, parseSuccess) => {
    fastForEach(parseSuccess.topLevelElements, (tle) => {
      const debugPath = [filePath]
      extractUIDFromTopLevelElement(workingResult, filePath, debugPath, tle)
    })
  })

  return getAllUniqueUIDsResultFromWorkingResult(workingResult)
}

export const getAllUniqueUids = memoize(getAllUniqueUidsInnerOld)

export function getAllUniqueUidsFromAttributes(attributes: JSXAttributes): Array<string> {
  const workingResult = emptyGetAllUniqueUIDsWorkingResult()

  const debugPath: Array<string> = []
  extractUidFromAttributes(workingResult, '', debugPath, attributes) // FIXME filePath

  return getAllUniqueUidsFromLookup(
    getAllUniqueUIDsResultFromWorkingResult(workingResult).filePathToUids,
  )
}

export function getAllUniqueUIdsFromElementChild(
  expression: JSXElementChild,
): GetAllUniqueUIDsResult {
  const workingResult = emptyGetAllUniqueUIDsWorkingResult()

  const debugPath: Array<string> = []
  extractUidFromJSXElementChild(workingResult, '', debugPath, expression) // FIXME filePath

  return getAllUniqueUIDsResultFromWorkingResult(workingResult)
}
