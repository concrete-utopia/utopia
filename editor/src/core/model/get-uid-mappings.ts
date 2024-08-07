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
import type { ParseSuccess } from 'utopia-shared/src/types'
import { mapFirstApplicable } from '../shared/array-utils'

export type DuplicateUIDsResult = Map<string, string>

interface GetAllUniqueUIDsResult {
  duplicateIDs: DuplicateUIDsResult
  filePathToUids: FileToUidMapping
}

interface GetAllUniqueUidsResultForFile {
  uniqueIDs: Set<string>
  duplicateIDs: DuplicateUIDsResult
}

function checkUID(
  working: Set<string>,
  duplicatedIDs: DuplicateUIDsResult,
  filePath: string,
  uid: string,
): void {
  if (working.has(uid)) {
    duplicatedIDs.set(uid, filePath)
  }

  working.add(uid)
}

function extractUidFromAttributes(
  working: Set<string>,
  workingDupliactedUIDs: DuplicateUIDsResult,
  filePath: string,
  attributes: JSXAttributes,
): void {
  for (const attributePart of attributes) {
    switch (attributePart.type) {
      case 'JSX_ATTRIBUTES_ENTRY':
        extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, attributePart.value)
        break
      case 'JSX_ATTRIBUTES_SPREAD':
        extractUidFromJSXElementChild(
          working,
          workingDupliactedUIDs,
          filePath,
          attributePart.spreadValue,
        )
        break
      default:
        assertNever(attributePart)
    }
  }
}

function extractUidFromJSXElementChild(
  working: Set<string>,
  workingDupliactedUIDs: DuplicateUIDsResult,
  filePath: string,
  element: JSXElementChild,
): void {
  checkUID(working, workingDupliactedUIDs, filePath, element.uid) // TODO handle duplicate UID
  switch (element.type) {
    case 'JSX_ELEMENT':
      fastForEach(element.children, (child) =>
        extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, child),
      )
      extractUidFromAttributes(working, workingDupliactedUIDs, filePath, element.props)
      break
    case 'JSX_FRAGMENT':
      fastForEach(element.children, (child) =>
        extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, child),
      )
      break
    case 'JSX_CONDITIONAL_EXPRESSION':
      extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, element.condition)
      extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, element.whenTrue)
      extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, element.whenFalse)
      break
    case 'JSX_TEXT_BLOCK':
      break
    case 'ATTRIBUTE_VALUE':
      break
    case 'ATTRIBUTE_NESTED_ARRAY':
      for (const contentPart of element.content) {
        extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, contentPart.value)
      }
      break
    case 'ATTRIBUTE_NESTED_OBJECT':
      for (const contentPart of element.content) {
        extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, contentPart.value)
      }
      break
    case 'JSX_MAP_EXPRESSION':
      extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, element.valueToMap)
      extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, element.mapFunction)
      break
    case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      for (const elementWithin of Object.values(element.elementsWithin)) {
        extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, elementWithin)
      }
      break
    case 'ATTRIBUTE_FUNCTION_CALL':
      for (const parameter of element.parameters) {
        extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, parameter)
      }
      break
    case 'JS_IDENTIFIER':
      break
    case 'JS_PROPERTY_ACCESS':
      extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, element.onValue)
      break
    case 'JS_ELEMENT_ACCESS':
      extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, element.onValue)
      extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, element.element)
      break
    default:
      assertNever(element)
  }
}

function extractUIDFromArbitraryBlock(
  working: Set<string>,
  workingDupliactedUIDs: DuplicateUIDsResult,
  filePath: string,
  arbitraryBlock: ArbitraryJSBlock,
): void {
  checkUID(working, workingDupliactedUIDs, filePath, arbitraryBlock.uid)
  for (const elementWithin of Object.values(arbitraryBlock.elementsWithin)) {
    extractUidFromJSXElementChild(working, workingDupliactedUIDs, filePath, elementWithin)
  }
}

export function extractUIDFromTopLevelElement(
  working: Set<string>,
  workingDupliactedUIDs: DuplicateUIDsResult,
  filePath: string,
  topLevelElement: TopLevelElement,
): void {
  switch (topLevelElement.type) {
    case 'UTOPIA_JSX_COMPONENT':
      extractUidFromJSXElementChild(
        working,
        workingDupliactedUIDs,
        filePath,
        topLevelElement.rootElement,
      )
      if (topLevelElement.arbitraryJSBlock != null) {
        extractUIDFromArbitraryBlock(
          working,
          workingDupliactedUIDs,
          filePath,
          topLevelElement.arbitraryJSBlock,
        )
      }
      break
    case 'ARBITRARY_JS_BLOCK':
      extractUIDFromArbitraryBlock(working, workingDupliactedUIDs, filePath, topLevelElement)
      break
    case 'IMPORT_STATEMENT':
      break
    case 'UNPARSED_CODE':
      break
    default:
      assertNever(topLevelElement)
  }
}

type FileToUidMapping = Map<string, Set<string>>

let CachedUidsPerFile = new WeakMap<ParseSuccess, GetAllUniqueUidsResultForFile>()

function collectUidsForFile(
  filePath: string,
  parseSuccess: ParseSuccess,
): GetAllUniqueUidsResultForFile {
  const cachedResult = CachedUidsPerFile.get(parseSuccess)
  if (cachedResult != null) {
    return cachedResult
  }

  let workingUniqueIDSet = emptySet<string>()
  let workingDupliactedUIDs: DuplicateUIDsResult = new Map()
  fastForEach(parseSuccess.topLevelElements, (tle) => {
    extractUIDFromTopLevelElement(workingUniqueIDSet, workingDupliactedUIDs, filePath, tle)
  })
  const result = { uniqueIDs: workingUniqueIDSet, duplicateIDs: workingDupliactedUIDs }
  CachedUidsPerFile.set(parseSuccess, result)
  return result
}

function collectUidsForEachFile(projectContents: ProjectContentTreeRoot): GetAllUniqueUIDsResult {
  let workingFileToUidMapping: FileToUidMapping = new Map()
  let workingDupliactedUIDs: DuplicateUIDsResult = new Map()
  walkContentsTreeForParseSuccess(projectContents, (filePath, parseSuccess) => {
    const mappingsForFile = collectUidsForFile(filePath, parseSuccess)
    // mappingsForFile.uniqueIDs.forEach((uid) => {
    //   workingAllUids.add(uid)
    // })
    workingFileToUidMapping.set(filePath, mappingsForFile.uniqueIDs)
    mappingsForFile.duplicateIDs.forEach((file, uid) => {
      workingDupliactedUIDs.set(uid, file)
    })
  })
  return {
    filePathToUids: workingFileToUidMapping,
    duplicateIDs: workingDupliactedUIDs,
  }
}

export function clearCachedUidsPerFileForTests() {
  CachedUidsPerFile = new WeakMap()
}

export function getFilePathForUid(mapping: FileToUidMapping, uid: string): string | null {
  // let result: string | null | undefined = null
  return mapFirstApplicable(mapping.entries(), ([filePath, uidMapping]) => {
    if (uidMapping.has(uid)) {
      return filePath
    }
    return null
  })
}

export function getUniqueUidsMappingInner(
  projectContents: ProjectContentTreeRoot,
): GetAllUniqueUIDsResult {
  return collectUidsForEachFile(projectContents)
}

export const getUidMappings = memoize(getUniqueUidsMappingInner)

export function getAllUniqueUidsFromMapping(mapping: FileToUidMapping): Array<string> {
  let result: Array<string> = []
  for (const [filePath, uids] of mapping.entries()) {
    result.push(...Array.from(uids))
  }
  return result
}

export function getAllUniqueUIdsFromElementChild(expression: JSXElementChild): {
  allUids: Set<string>
  duplicateIDs: DuplicateUIDsResult
} {
  let workingUniqueIDSet = emptySet<string>()
  let workingDupliactedUIDs: DuplicateUIDsResult = new Map()
  extractUidFromJSXElementChild(workingUniqueIDSet, workingDupliactedUIDs, '', expression) // FIXME filePath

  return {
    allUids: workingUniqueIDSet,
    duplicateIDs: workingDupliactedUIDs,
  }
}

export function getAllUniqueUidsFromAttributes(attributes: JSXAttributes): {
  allUids: Set<string>
  duplicateIDs: DuplicateUIDsResult
} {
  let workingUniqueIDSet = emptySet<string>()
  let workingDupliactedUIDs: DuplicateUIDsResult = new Map()
  extractUidFromAttributes(workingUniqueIDSet, workingDupliactedUIDs, '', attributes) // FIXME filePath

  return {
    allUids: workingUniqueIDSet,
    duplicateIDs: workingDupliactedUIDs,
  }
}
