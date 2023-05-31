import { ProjectContentTreeRoot, walkContentsTreeForParseSuccess } from '../../components/assets'
import {
  ArbitraryJSBlock,
  JSXAttributes,
  JSXElementChild,
  TopLevelElement,
} from '../shared/element-template'
import { emptySet } from '../shared/set-utils'
import { assertNever, fastForEach } from '../shared/utils'
import Utils from '../../utils/utils'

interface GetAllUniqueUIDsResult {
  uniqueIDs: Array<string>
  duplicateIDs: { [key: string]: Array<Array<string>> }
  allIDs: Array<string>
}

interface GetAllUniqueUIDsWorkingResult {
  uniqueIDs: { [key: string]: Array<string> }
  duplicateIDs: { [key: string]: Array<Array<string>> }
  allIDs: Set<string>
}

function emptyGetAllUniqueUIDsWorkingResult(): GetAllUniqueUIDsWorkingResult {
  return {
    uniqueIDs: {},
    duplicateIDs: {},
    allIDs: emptySet(),
  }
}

function getAllUniqueUIDsResultFromWorkingResult(
  workingResult: GetAllUniqueUIDsWorkingResult,
): GetAllUniqueUIDsResult {
  return {
    uniqueIDs: Object.keys(workingResult.uniqueIDs),
    duplicateIDs: workingResult.duplicateIDs,
    allIDs: Array.from(workingResult.allIDs),
  }
}

function checkUID(
  workingResult: GetAllUniqueUIDsWorkingResult,
  debugPath: Array<string>,
  uid: string,
  value: any,
): void {
  workingResult.allIDs.add(uid)
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
  debugPath: Array<string>,
  attributes: JSXAttributes,
): void {
  for (const attributePart of attributes) {
    switch (attributePart.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        extractUid(workingResult, debugPath, attributePart.value)
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        extractUid(workingResult, debugPath, attributePart.spreadValue)
        break
      default:
        assertNever(attributePart)
    }
  }
}

function extractUid(
  workingResult: GetAllUniqueUIDsWorkingResult,
  debugPath: Array<string>,
  element: JSXElementChild,
): void {
  const newDebugPath = [...debugPath, element.uid]
  checkUID(workingResult, newDebugPath, element.uid, element)
  switch (element.type) {
    case 'JSX_ELEMENT':
      fastForEach(element.children, (child) => extractUid(workingResult, newDebugPath, child))
      extractUidFromAttributes(workingResult, newDebugPath, element.props)
      break
    case 'JSX_FRAGMENT':
      fastForEach(element.children, (child) => extractUid(workingResult, newDebugPath, child))
      break
    case 'JSX_CONDITIONAL_EXPRESSION':
      extractUid(workingResult, newDebugPath, element.condition)
      extractUid(workingResult, newDebugPath, element.whenTrue)
      extractUid(workingResult, newDebugPath, element.whenFalse)
      break
    case 'JSX_TEXT_BLOCK':
      break
    case 'ATTRIBUTE_VALUE':
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      for (const contentPart of element.content) {
        extractUid(workingResult, newDebugPath, contentPart.value)
      }
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      for (const contentPart of element.content) {
        extractUid(workingResult, newDebugPath, contentPart.value)
      }
      break
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      for (const elementWithin of Object.values(element.elementsWithin)) {
        extractUid(workingResult, newDebugPath, elementWithin)
      }
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      for (const parameter of element.parameters) {
        extractUid(workingResult, newDebugPath, parameter)
      }
      break
    default:
      assertNever(element)
  }
}

function extractUIDFromArbitraryBlock(
  workingResult: GetAllUniqueUIDsWorkingResult,
  debugPath: Array<string>,
  arbitraryBlock: ArbitraryJSBlock,
): void {
  const newDebugPath = [...debugPath, arbitraryBlock.uid]
  checkUID(workingResult, newDebugPath, arbitraryBlock.uid, arbitraryBlock)
  for (const elementWithin of Object.values(arbitraryBlock.elementsWithin)) {
    extractUid(workingResult, newDebugPath, elementWithin)
  }
}

function extractUIDFromTopLevelElement(
  workingResult: GetAllUniqueUIDsWorkingResult,
  debugPath: Array<string>,
  topLevelElement: TopLevelElement,
): void {
  switch (topLevelElement.type) {
    case 'UTOPIA_JSX_COMPONENT':
      extractUid(workingResult, debugPath, topLevelElement.rootElement)
      if (topLevelElement.arbitraryJSBlock != null) {
        extractUIDFromArbitraryBlock(workingResult, debugPath, topLevelElement.arbitraryJSBlock)
      }
      break
    case 'ARBITRARY_JS_BLOCK':
      extractUIDFromArbitraryBlock(workingResult, debugPath, topLevelElement)
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

  walkContentsTreeForParseSuccess(projectContents, (fullPath, parseSuccess) => {
    fastForEach(parseSuccess.topLevelElements, (tle) => {
      const debugPath = [fullPath]
      extractUIDFromTopLevelElement(workingResult, debugPath, tle)
    })
  })

  return getAllUniqueUIDsResultFromWorkingResult(workingResult)
}

export const getAllUniqueUids = Utils.memoize(getAllUniqueUidsInner)

export function getAllUniqueUidsFromAttributes(attributes: JSXAttributes): GetAllUniqueUIDsResult {
  const workingResult = emptyGetAllUniqueUIDsWorkingResult()

  const debugPath: Array<string> = []
  extractUidFromAttributes(workingResult, debugPath, attributes)

  return getAllUniqueUIDsResultFromWorkingResult(workingResult)
}

export function getAllUniqueUIdsFromElementChild(
  expression: JSXElementChild,
): GetAllUniqueUIDsResult {
  const workingResult = emptyGetAllUniqueUIDsWorkingResult()

  const debugPath: Array<string> = []
  extractUid(workingResult, debugPath, expression)

  return getAllUniqueUIDsResultFromWorkingResult(workingResult)
}
