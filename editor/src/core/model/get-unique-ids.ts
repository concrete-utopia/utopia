import type { ProjectContentTreeRoot } from '../../components/assets'
import { walkContentsTreeForParseSuccess } from '../../components/assets'
import type { UtopiaJSXComponent } from '../shared/element-template'
import {
  uidFromElementChild,
  type ArbitraryJSBlock,
  type JSXAttributes,
  type JSXElementChild,
  type TopLevelElement,
} from '../shared/element-template'
import { emptySet } from '../shared/set-utils'
import { assertNever } from '../shared/utils'
import { memoize } from '../shared/memoize'

export type DuplicateUIDsResult = { [key: string]: Array<Array<string>> }

interface GetAllUniqueUIDsResult {
  uniqueIDs: Array<string>
  duplicateIDs: DuplicateUIDsResult
  allIDs: Array<string>
  uidsToFilePaths: { [uid: string]: string }
}

interface GetAllUniqueUIDsWorkingResult {
  uniqueIDs: { [key: string]: Array<string> }
  duplicateIDs: { [key: string]: Array<Array<string>> }
  allIDs: Set<string>
  uidsToFilePaths: { [uid: string]: string }
}

export function emptyGetAllUniqueUIDsWorkingResult(): GetAllUniqueUIDsWorkingResult {
  return {
    uniqueIDs: {},
    duplicateIDs: {},
    allIDs: emptySet(),
    uidsToFilePaths: {},
  }
}

export function getAllUniqueUIDsResultFromWorkingResult(
  workingResult: GetAllUniqueUIDsWorkingResult,
): GetAllUniqueUIDsResult {
  return {
    uniqueIDs: Object.keys(workingResult.uniqueIDs),
    duplicateIDs: workingResult.duplicateIDs,
    allIDs: Array.from(workingResult.allIDs),
    uidsToFilePaths: workingResult.uidsToFilePaths,
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
  workingResult.uidsToFilePaths[uid] = filePath
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
  const newDebugPath = [...debugPath, uidFromElementChild(element)]
  // `UTOPIA_JSX_COMPONENT` shouldn't double dip by also checking the `rootElement`, so
  // exclude it here.
  if (element.type !== 'UTOPIA_JSX_COMPONENT') {
    checkUID(workingResult, filePath, newDebugPath, element.uid, element)
  }
  switch (element.type) {
    case 'JSX_ELEMENT':
      for (const child of element.children) {
        extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, child)
      }
      extractUidFromAttributes(workingResult, filePath, newDebugPath, element.props)
      break
    case 'JSX_FRAGMENT':
      for (const child of element.children) {
        extractUidFromJSXElementChild(workingResult, filePath, newDebugPath, child)
      }
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
    case 'UTOPIA_JSX_COMPONENT':
      extractUIDFromComponent(workingResult, filePath, debugPath, element)
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

export function extractUIDFromComponent(
  workingResult: GetAllUniqueUIDsWorkingResult,
  filePath: string,
  debugPath: Array<string>,
  component: UtopiaJSXComponent,
): void {
  extractUidFromJSXElementChild(workingResult, filePath, debugPath, component.rootElement)
  if (component.arbitraryJSBlock != null) {
    extractUIDFromArbitraryBlock(workingResult, filePath, debugPath, component.arbitraryJSBlock)
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
      extractUIDFromComponent(workingResult, filePath, debugPath, topLevelElement)
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

function getAllUniqueUidsInner(projectContents: ProjectContentTreeRoot): GetAllUniqueUIDsResult {
  const workingResult = emptyGetAllUniqueUIDsWorkingResult()

  walkContentsTreeForParseSuccess(projectContents, (filePath, parseSuccess) => {
    for (const tle of parseSuccess.topLevelElements) {
      const debugPath = [filePath]
      extractUIDFromTopLevelElement(workingResult, filePath, debugPath, tle)
    }
  })

  return getAllUniqueUIDsResultFromWorkingResult(workingResult)
}

export const getAllUniqueUids = memoize(getAllUniqueUidsInner)

export function getAllUniqueUidsFromAttributes(attributes: JSXAttributes): GetAllUniqueUIDsResult {
  const workingResult = emptyGetAllUniqueUIDsWorkingResult()

  const debugPath: Array<string> = []
  extractUidFromAttributes(workingResult, '', debugPath, attributes) // FIXME filePath

  return getAllUniqueUIDsResultFromWorkingResult(workingResult)
}

export function getAllUniqueUIdsFromElementChild(
  expression: JSXElementChild,
): GetAllUniqueUIDsResult {
  const workingResult = emptyGetAllUniqueUIDsWorkingResult()

  const debugPath: Array<string> = []
  extractUidFromJSXElementChild(workingResult, '', debugPath, expression) // FIXME filePath

  return getAllUniqueUIDsResultFromWorkingResult(workingResult)
}
