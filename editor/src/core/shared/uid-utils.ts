import { v4 as UUID } from 'uuid'
import { UTOPIA_PATH_KEY } from '../model/utopia-constants'
import { mapDropNulls } from './array-utils'
import { deepFindUtopiaCommentFlag, isUtopiaCommentFlagUid } from './comment-flags'
import { getDOMAttribute } from './dom-utils'
import type { Either } from './either'
import { flatMapEither, isRight, left, right } from './either'
import * as EP from './element-path'
import type {
  ElementInstanceMetadata,
  JSXAttributes,
  JSXElement,
  JSXElementChild,
  JSXFragment,
  JSXTextBlock,
  ParsedComments,
  TopLevelElement,
  JSExpressionOtherJavaScript,
  JSExpressionValue,
  JSExpressionNestedArray,
  JSExpressionNestedObject,
  JSExpressionFunctionCall,
  JSXMapExpression,
} from './element-template'
import {
  emptyComments,
  isJSXAttributeValue,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXFragment,
  isJSXTextBlock,
  jsExpressionValue,
  jsxConditionalExpression,
  jsxElement,
  jsxFragment,
  setJSXAttributesAttribute,
  modifiableAttributeIsAttributeNestedArray,
  modifiableAttributeIsAttributeNestedObject,
  modifiableAttributeIsAttributeFunctionCall,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsExpressionFunctionCall,
  jsExpressionOtherJavaScript,
  isJSExpressionOtherJavaScript,
  isJSXMapExpression,
  jsxMapExpression,
} from './element-template'
import type { ElementPath, HighlightBoundsForUids } from './project-file-types'
import * as PP from './property-path'
import { emptySet } from './set-utils'
import { getModifiableJSXAttributeAtPath, jsxSimpleAttributeToValue } from './jsx-attribute-utils'
import { hashObject } from './hash'
import type { LineAndCharacter } from 'typescript'

export const MOCK_NEXT_GENERATED_UIDS: { current: Array<string> } = { current: [] }
export const MOCK_NEXT_GENERATED_UIDS_IDX = { current: 0 }

export function generateMockNextGeneratedUID(): string | null {
  if (
    MOCK_NEXT_GENERATED_UIDS.current.length > 0 &&
    MOCK_NEXT_GENERATED_UIDS_IDX.current < MOCK_NEXT_GENERATED_UIDS.current.length
  ) {
    const nextID = MOCK_NEXT_GENERATED_UIDS.current[MOCK_NEXT_GENERATED_UIDS_IDX.current]
    MOCK_NEXT_GENERATED_UIDS_IDX.current += 1
    return nextID
  } else {
    return null
  }
}

export const UtopiaIDPropertyPath = PP.create('data-uid')

export const atoz = [
  'a',
  'b',
  'c',
  'd',
  'e',
  'f',
  'g',
  'h',
  'i',
  'j',
  'k',
  'l',
  'm',
  'n',
  'o',
  'p',
  'q',
  'r',
  's',
  't',
  'u',
  'v',
  'w',
  'x',
  'y',
  'z',
]

export function updateHighlightBounds(
  highlightBounds: HighlightBoundsForUids,
  mappings: UIDMappings,
): HighlightBoundsForUids {
  let result: HighlightBoundsForUids = {}
  let movedOriginalUIDs: Set<string> = emptySet()
  // Move the bounds for the mappings first, tracking what we have moved in `movedOriginalUIDs`.
  for (const mapping of mappings) {
    const { originalUID, newUID } = mapping
    if (originalUID in highlightBounds) {
      const bounds = highlightBounds[originalUID]
      movedOriginalUIDs.add(originalUID)
      result[newUID] = {
        ...bounds,
        uid: newUID,
      }
    }
  }

  // Move over the remainder, which aren't in `movedOriginalUIDs`.
  for (const [uid, bounds] of Object.entries(highlightBounds)) {
    if (!movedOriginalUIDs.has(uid)) {
      result[uid] = bounds
    }
  }

  return result
}

interface Bounds {
  start: LineAndCharacter
  end: LineAndCharacter
}

export function generateHashUID(data: {
  fileName: string
  bounds: Bounds | null
  value: any
}): string {
  const hash = hashObject(data)
  return uidOrMockUid(hash)
}

export function generateUID(): string {
  const uid = UUID().replace(/\-/g, '')
  return uidOrMockUid(uid)
}

function uidOrMockUid(uid: string): string {
  const mockUID = generateMockNextGeneratedUID()
  if (mockUID == null) {
    return uid
  } else {
    return mockUID
  }
}

export function parseUIDFromComments(comments: ParsedComments): Either<string, string> {
  const commentFlag = deepFindUtopiaCommentFlag(comments ?? null, 'uid')
  if (commentFlag != null && isUtopiaCommentFlagUid(commentFlag)) {
    const { value } = commentFlag
    return right(value)
  } else {
    return left('Unable to find or parse uid comment.')
  }
}

export function parseUID(
  attributes: JSXAttributes,
  comments: ParsedComments,
): Either<string, string> {
  const fromComments = parseUIDFromComments(comments)
  if (isRight(fromComments)) {
    return fromComments
  }

  const uidAttribute = getModifiableJSXAttributeAtPath(attributes, UtopiaIDPropertyPath)
  const uidValue = flatMapEither(jsxSimpleAttributeToValue, uidAttribute)
  return flatMapEither((uid) => {
    if (typeof uid === 'string') {
      return right(uid)
    } else {
      return left('Unexpected data-uid value.')
    }
  }, uidValue)
}

export function getUtopiaIDFromJSXElement(element: JSXElementChild): string {
  return element.uid
}

export type UIDMappings = Array<{ originalUID: string; newUID: string }>

export interface WithUIDMappings<T> {
  mappings: UIDMappings
  value: T
}

function getSplitPathsStrings(pathsString: string | null): Array<string> {
  return pathsString == null ? [] : EP.getAllElementPathStringsForPathString(pathsString)
}

function getPathsFromSplitString(splitPaths: Array<string>): Array<ElementPath> {
  return splitPaths.map(EP.fromString).filter(EP.isElementPath)
}

export function getPathsFromString(pathsString: string | null): Array<ElementPath> {
  return getPathsFromSplitString(getSplitPathsStrings(pathsString))
}

export interface PathWithString {
  path: ElementPath
  asString: string
}

export function getPathWithStringsOnDomElement(element: Element): Array<PathWithString> {
  const pathsAttribute = getDOMAttribute(element, UTOPIA_PATH_KEY)
  return mapDropNulls((pathString) => {
    const parsedPath = EP.fromString(pathString)
    if (EP.isElementPath(parsedPath)) {
      return {
        path: parsedPath,
        asString: pathString,
      }
    } else {
      return null
    }
  }, getSplitPathsStrings(pathsAttribute))
}

export function getPathsOnDomElement(element: Element): Array<ElementPath> {
  const pathsAttribute = getDOMAttribute(element, UTOPIA_PATH_KEY)
  return getPathsFromString(pathsAttribute)
}

export function getPathStringsOnDomElement(element: Element): Array<string> {
  const pathsAttribute = getDOMAttribute(element, UTOPIA_PATH_KEY)
  return getSplitPathsStrings(pathsAttribute)
}

export function getDeepestPathOnDomElement(element: Element): ElementPath | null {
  const pathAttribute = getDOMAttribute(element, UTOPIA_PATH_KEY)
  return pathAttribute == null ? null : EP.fromString(pathAttribute)
}

export function findElementWithUID(
  topLevelElement: TopLevelElement,
  targetUID: string,
): JSXElement | null {
  function findForJSXElementChild(element: JSXElementChild): JSXElement | null {
    switch (element.type) {
      case 'JSX_ELEMENT':
        return findForJSXElement(element)
      case 'JSX_FRAGMENT':
        for (const child of element.children) {
          const childResult = findForJSXElementChild(child)
          if (childResult != null) {
            return childResult
          }
        }
        return null
      case 'JSX_TEXT_BLOCK':
        return null
      case 'JSX_MAP_EXPRESSION':
        return (
          findForJSXElementChild(element.valueToMap) ?? findForJSXElementChild(element.mapFunction)
        )
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        if (targetUID in element.elementsWithin) {
          return element.elementsWithin[targetUID]
        }
        for (const elementWithin of Object.values(element.elementsWithin)) {
          const elementWithinResult = findForJSXElement(elementWithin)
          if (elementWithinResult != null) {
            return elementWithinResult
          }
        }
        return null
      case 'JSX_CONDITIONAL_EXPRESSION':
        const findResultWhenTrue = findForJSXElementChild(element.whenTrue)
        if (findResultWhenTrue != null) {
          return findResultWhenTrue
        }
        const findResultWhenFalse = findForJSXElementChild(element.whenFalse)
        if (findResultWhenFalse != null) {
          return findResultWhenFalse
        }
        return null
      case 'ATTRIBUTE_VALUE':
      case 'JS_IDENTIFIER':
        return null
      case 'JS_ELEMENT_ACCESS': {
        const findResultOnValue = findForJSXElementChild(element.onValue)
        if (findResultOnValue != null) {
          return findResultOnValue
        }
        const findResultElement = findForJSXElementChild(element.element)
        if (findResultElement != null) {
          return findResultElement
        }
        return null
      }
      case 'JS_PROPERTY_ACCESS': {
        const findResultOnValue = findForJSXElementChild(element.onValue)
        if (findResultOnValue != null) {
          return findResultOnValue
        }
        return null
      }
      case 'ATTRIBUTE_NESTED_ARRAY':
        for (const contentElement of element.content) {
          const elementWithinResult = findForJSXElementChild(contentElement.value)
          if (elementWithinResult != null) {
            return elementWithinResult
          }
        }
        return null
      case 'ATTRIBUTE_NESTED_OBJECT':
        for (const contentElement of element.content) {
          const elementWithinResult = findForJSXElementChild(contentElement.value)
          if (elementWithinResult != null) {
            return elementWithinResult
          }
        }
        return null
      case 'ATTRIBUTE_FUNCTION_CALL':
        for (const parameter of element.parameters) {
          const elementWithinResult = findForJSXElementChild(parameter)
          if (elementWithinResult != null) {
            return elementWithinResult
          }
        }
        return null
      default:
        const _exhaustiveCheck: never = element
        throw new Error(`Unhandled element type ${JSON.stringify(element)}`)
    }
  }

  function findForJSXElement(element: JSXElement): JSXElement | null {
    const uid = getUtopiaIDFromJSXElement(element)
    if (uid === targetUID) {
      return element
    } else {
      for (const child of element.children) {
        const childResult = findForJSXElementChild(child)
        if (childResult != null) {
          return childResult
        }
      }
    }
    return null
  }

  switch (topLevelElement.type) {
    case 'UTOPIA_JSX_COMPONENT':
      return findForJSXElementChild(topLevelElement.rootElement)
    case 'ARBITRARY_JS_BLOCK':
      return null
    case 'UNPARSED_CODE':
      return null
    case 'IMPORT_STATEMENT':
      return null
  }
}

// THIS IS SUPER UGLY, DO NOT USE OUTSIDE OF FILE
function isUtopiaJSXElement(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSXElement {
  return isJSXElement(element as any)
}

function isUtopiaJSExpressionOtherJavaScript(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSExpressionOtherJavaScript {
  return isJSExpressionOtherJavaScript(element as any)
}

function isUtopiaJSXMapExpression(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSXMapExpression {
  return isJSXMapExpression(element as any)
}

function isUtopiaJSExpressionValue(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSExpressionValue<any> {
  return isJSXAttributeValue(element as any)
}

function isUtopiaJSExpressionNestedArray(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSExpressionNestedArray {
  return modifiableAttributeIsAttributeNestedArray(element as any)
}

function isUtopiaJSExpressionNestedObject(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSExpressionNestedObject {
  return modifiableAttributeIsAttributeNestedObject(element as any)
}

function isUtopiaJSExpressionFunctionCall(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSExpressionFunctionCall {
  return modifiableAttributeIsAttributeFunctionCall(element as any)
}

function isUtopiaJSXTextBlock(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSXTextBlock {
  return isJSXTextBlock(element as any)
}

function isUtopiaJSXFragment(
  element: JSXElementChild | ElementInstanceMetadata,
): element is JSXFragment {
  return isJSXFragment(element as any)
}

function isElementInstanceMetadata(
  element: JSXElementChild | ElementInstanceMetadata,
): element is ElementInstanceMetadata {
  return (element as any).elementPath != null
}

export function setUtopiaID(element: JSXElementChild, uid: string): JSXElementChild {
  if (isUtopiaJSXElement(element)) {
    return jsxElement(
      element.name,
      uid,
      setJSXAttributesAttribute(element.props, 'data-uid', jsExpressionValue(uid, emptyComments)),
      element.children,
    )
  } else if (isUtopiaJSXFragment(element)) {
    return jsxFragment(uid, element.children, element.longForm)
  } else if (isJSXConditionalExpression(element)) {
    return jsxConditionalExpression(
      uid,
      element.condition,
      element.originalConditionString,
      element.whenTrue,
      element.whenFalse,
      element.comments,
    )
  } else if (isUtopiaJSExpressionValue(element)) {
    return jsExpressionValue(element.value, element.comments, uid)
  } else if (isUtopiaJSExpressionNestedArray(element)) {
    return jsExpressionNestedArray(element.content, element.comments, uid)
  } else if (isUtopiaJSExpressionNestedObject(element)) {
    return jsExpressionNestedObject(element.content, element.comments, uid)
  } else if (isUtopiaJSExpressionFunctionCall(element)) {
    return jsExpressionFunctionCall(element.functionName, element.parameters, uid)
  } else if (isUtopiaJSExpressionOtherJavaScript(element)) {
    return jsExpressionOtherJavaScript(
      element.params,
      element.originalJavascript,
      element.javascriptWithUIDs,
      element.transpiledJavascript,
      element.definedElsewhere,
      element.sourceMap,
      element.elementsWithin,
      element.comments,
      uid,
    )
  } else if (isUtopiaJSXMapExpression(element)) {
    return jsxMapExpression(
      element.valueToMap,
      element.mapFunction,
      element.comments,
      element.valuesInScopeFromParameters,
      uid,
    )
  } else {
    throw new Error(`Unable to set utopia id on ${element.type}`)
  }
}

export function getUtopiaID(element: JSXElementChild | ElementInstanceMetadata): string {
  if (isElementInstanceMetadata(element)) {
    return EP.toUid(element.elementPath)
  }
  return element.uid
}
