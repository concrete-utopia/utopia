import { v4 as UUID } from 'uuid'
import { UTOPIA_PATH_KEY } from '../model/utopia-constants'
import { mapDropNulls } from './array-utils'
import { deepFindUtopiaCommentFlag, isUtopiaCommentFlagUid } from './comment-flags'
import { getDOMAttribute } from './dom-utils'
import type { Either } from './either'
import { flatMapEither, foldEither, isLeft, isRight, left, right } from './either'
import * as EP from './element-path'
import type {
  ElementInstanceMetadata,
  JSExpression,
  JSXAttributes,
  JSXConditionalExpression,
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
  JSXArrayElement,
  JSXProperty,
  JSExpressionMapOrOtherJavascript,
  JSXMapExpression,
  JSIdentifier,
  JSPropertyAccess,
  JSElementAccess,
} from './element-template'
import {
  emptyComments,
  getJSXAttribute,
  isJSExpressionMapOrOtherJavaScript,
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
  isJSExpression,
  isJSExpressionOtherJavaScript,
  isJSXMapExpression,
  jsxMapExpression,
} from './element-template'
import { shallowEqual } from './equality-utils'
import { objectMap } from './object-utils'
import type { ElementPath, HighlightBoundsForUids } from './project-file-types'
import * as PP from './property-path'
import { assertNever } from './utils'
import fastDeepEquals from 'fast-deep-equal'
import { emptySet } from './set-utils'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
} from './jsx-attribute-utils'

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

// Assumes possibleStartingValue consists only of characters that are valid to begin with.
export function generateConsistentUID(
  possibleStartingValue: string,
  ...existingIDSets: Array<Set<string>>
): string {
  function alreadyExistingID(idToCheck: string): boolean {
    return existingIDSets.some((s) => s.has(idToCheck))
  }
  const mockUID = generateMockNextGeneratedUID()
  if (mockUID == null) {
    if (possibleStartingValue.length >= 3) {
      const maxSteps = Math.floor(possibleStartingValue.length / 3)
      for (let step = 0; step < maxSteps; step++) {
        const possibleUID = possibleStartingValue.substring(step * 3, (step + 1) * 3)

        if (!alreadyExistingID(possibleUID)) {
          return possibleUID
        }
      }
    }

    for (let firstChar of atoz) {
      for (let secondChar of atoz) {
        for (let thirdChar of atoz) {
          const possibleUID = `${firstChar}${secondChar}${thirdChar}`

          if (!alreadyExistingID(possibleUID)) {
            return possibleUID
          }
        }
      }
    }

    // Fallback bailout.
    throw new Error(`Unable to generate a UID from '${possibleStartingValue}'.`)
  } else {
    return mockUID
  }
}

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

export function generateUID(existingIDs: Array<string> | Set<string>): string {
  const mockUID = generateMockNextGeneratedUID()
  if (mockUID == null) {
    const fullUid = UUID().replace(/\-/g, '')
    // trying to find a new 3 character substring from the full uid
    for (let i = 0; i < fullUid.length - 3; i++) {
      const id = fullUid.substring(i, i + 3)
      if (Array.isArray(existingIDs)) {
        if (!existingIDs.includes(id)) {
          return id
        }
      } else {
        if (!existingIDs.has(id)) {
          return id
        }
      }
    }
    // if all the substrings are already used as ids, let's try again with a new full uid
    return generateUID(existingIDs)
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

export function fixUtopiaExpression(
  expressionToFix: JSExpression,
  uniqueIDsMutable: Set<string>,
): WithUIDMappings<JSExpression> {
  const fixedAsElementResult = fixUtopiaElement(expressionToFix, uniqueIDsMutable)
  if (isJSExpression(fixedAsElementResult.value)) {
    return {
      mappings: fixedAsElementResult.mappings,
      value: fixedAsElementResult.value,
    }
  } else {
    throw new Error(`Got an element back instead of an expression unexpectedly.`)
  }
}

export function fixUtopiaElementGeneric<T extends JSXElementChild>(
  elementToFix: T,
  uniqueIDsMutable: Set<string>,
): WithUIDMappings<T> {
  const result = fixUtopiaElement(elementToFix, uniqueIDsMutable)
  if (result.value.type === elementToFix.type) {
    return result as WithUIDMappings<T>
  } else {
    throw new Error(`Switched type from ${elementToFix.type} to ${result.value.type}.`)
  }
}

export function fixUtopiaElement(
  elementToFix: JSXElementChild,
  uniqueIDsMutable: Set<string>,
): WithUIDMappings<JSXElementChild> {
  let mappings: Array<{ originalUID: string; newUID: string }> = []

  function fixAttributes(attributesToFix: JSXAttributes): JSXAttributes {
    return attributesToFix.map((attributeToFix) => {
      switch (attributeToFix.type) {
        case 'JSX_ATTRIBUTES_ENTRY':
          const updatedValue = fixJSExpression(attributeToFix.value)
          return {
            ...attributeToFix,
            value: updatedValue,
          }
        case 'JSX_ATTRIBUTES_SPREAD':
          const updatedSpreadValue = fixJSExpression(attributeToFix.spreadValue)
          return {
            ...attributeToFix,
            spreadValue: updatedSpreadValue,
          }
        default:
          assertNever(attributeToFix)
      }
    })
  }

  function fixJSXElement(element: JSXElement): JSXElement {
    let fixedChildren = element.children.map((elem) => fixUtopiaElementInner(elem))
    if (shallowEqual(element.children, fixedChildren)) {
      // saving reference equality in case the children didn't need fixing
      fixedChildren = element.children
    }

    let fixedProps = fixAttributes(element.props)
    if (fastDeepEquals(fixedProps, element.props)) {
      fixedProps = element.props
    }

    const uid = element.uid
    const uidProp = getJSXAttribute(fixedProps, 'data-uid')
    if (uidProp == null || !isJSXAttributeValue(uidProp) || uniqueIDsMutable.has(uid)) {
      const newUID = generateConsistentUID(uid, uniqueIDsMutable)
      mappings.push({ originalUID: uid, newUID: newUID })
      uniqueIDsMutable.add(newUID)
      const newUIDForProp = generateConsistentUID(uid, uniqueIDsMutable)
      if (uidProp != null) {
        mappings.push({ originalUID: uidProp.uid, newUID: newUIDForProp })
      }
      uniqueIDsMutable.add(newUIDForProp)
      const elementPropsWithNewUID = setJSXValueAtPath(
        fixedProps,
        UtopiaIDPropertyPath,
        jsExpressionValue(newUID, emptyComments, newUIDForProp),
      )

      return foldEither(
        (error) => {
          throw new Error(`Failed to add a uid to an element missing one ${error}`)
        },
        (propsWithNewUID) => {
          return jsxElement(element.name, newUID, propsWithNewUID, fixedChildren)
        },
        elementPropsWithNewUID,
      )
    } else if (element.children !== fixedChildren || element.props !== fixedProps) {
      uniqueIDsMutable.add(uid)
      return {
        ...element,
        children: fixedChildren,
        props: fixedProps,
      }
    } else {
      uniqueIDsMutable.add(uid)
      return element
    }
  }

  function addAndMaybeUpdateUID(currentUID: string): string {
    const fixedUID = uniqueIDsMutable.has(currentUID)
      ? generateConsistentUID(currentUID, uniqueIDsMutable)
      : currentUID
    if (fixedUID !== currentUID) {
      mappings.push({ originalUID: currentUID, newUID: fixedUID })
    }
    uniqueIDsMutable.add(fixedUID)
    return fixedUID
  }

  function fixJSXFragment(fragment: JSXFragment): JSXFragment {
    const fixedChildren = fragment.children.map((child) => fixUtopiaElementInner(child))
    const fixedUID = addAndMaybeUpdateUID(fragment.uid)
    return {
      ...fragment,
      uid: fixedUID,
      children: fixedChildren,
    }
  }

  function fixJSXConditionalExpression(
    conditional: JSXConditionalExpression,
  ): JSXConditionalExpression {
    const fixedUID = addAndMaybeUpdateUID(conditional.uid)
    return {
      ...conditional,
      uid: fixedUID,
      condition: fixJSExpression(conditional.condition),
      whenTrue: fixUtopiaElementInner(conditional.whenTrue),
      whenFalse: fixUtopiaElementInner(conditional.whenFalse),
    }
  }

  function fixJSXTextBlock(textBlock: JSXTextBlock): JSXTextBlock {
    const fixedUID = addAndMaybeUpdateUID(textBlock.uid)
    return {
      ...textBlock,
      uid: fixedUID,
    }
  }

  function fixJSFunctionCall(call: JSExpressionFunctionCall): JSExpressionFunctionCall {
    const fixedUID = addAndMaybeUpdateUID(call.uid)
    return {
      ...call,
      uid: fixedUID,
      parameters: call.parameters.map(fixJSExpression),
    }
  }

  function fixJSOtherJavaScript(
    otherJavaScript: JSExpressionMapOrOtherJavascript,
  ): JSExpressionMapOrOtherJavascript {
    const fixedUID = addAndMaybeUpdateUID(otherJavaScript.uid)
    return {
      ...otherJavaScript,
      uid: fixedUID,
      elementsWithin: objectMap(fixJSXElement, otherJavaScript.elementsWithin),
    }
  }

  function fixJSXArrayElement(element: JSXArrayElement): JSXArrayElement {
    switch (element.type) {
      case 'ARRAY_VALUE':
        return {
          ...element,
          value: fixJSExpression(element.value),
        }
      case 'ARRAY_SPREAD':
        return {
          ...element,
          value: fixJSExpression(element.value),
        }
      default:
        assertNever(element)
    }
  }

  function fixJSXProperty(element: JSXProperty): JSXProperty {
    switch (element.type) {
      case 'PROPERTY_ASSIGNMENT':
        return {
          ...element,
          value: fixJSExpression(element.value),
        }
      case 'SPREAD_ASSIGNMENT':
        return {
          ...element,
          value: fixJSExpression(element.value),
        }
      default:
        assertNever(element)
    }
  }

  function fixJSIdentifier(element: JSIdentifier): JSIdentifier {
    const fixedUID = addAndMaybeUpdateUID(element.uid)
    return {
      ...element,
      uid: fixedUID,
    }
  }

  function fixJSPropertyAccess(element: JSPropertyAccess): JSPropertyAccess {
    const fixedUID = addAndMaybeUpdateUID(element.uid)
    const fixedOnValue = fixJSExpression(element.onValue)
    return {
      ...element,
      uid: fixedUID,
      onValue: fixedOnValue,
    }
  }

  function fixJSElementAccess(element: JSElementAccess): JSElementAccess {
    const fixedUID = addAndMaybeUpdateUID(element.uid)
    const fixedOnValue = fixJSExpression(element.onValue)
    const fixedElement = fixJSExpression(element.element)
    return {
      ...element,
      uid: fixedUID,
      onValue: fixedOnValue,
      element: fixedElement,
    }
  }

  function fixJSExpression(value: JSExpression): JSExpression {
    switch (value.type) {
      case 'ATTRIBUTE_VALUE':
        return fixJSExpressionValue(value)
      case 'ATTRIBUTE_NESTED_ARRAY':
        return fixJSNestedArray(value)
      case 'ATTRIBUTE_NESTED_OBJECT':
        return fixJSNestedObject(value)
      case 'ATTRIBUTE_FUNCTION_CALL':
        return fixJSFunctionCall(value)
      case 'JSX_MAP_EXPRESSION':
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
        return fixJSOtherJavaScript(value)
      case 'JS_ELEMENT_ACCESS':
        return fixJSElementAccess(value)
      case 'JS_PROPERTY_ACCESS':
        return fixJSPropertyAccess(value)
      case 'JS_IDENTIFIER':
        return fixJSIdentifier(value)
      case 'JSX_ELEMENT':
        return fixJSXElement(value)
      default:
        assertNever(value)
    }
  }

  function fixJSExpressionValue(value: JSExpressionValue<any>): JSExpressionValue<any> {
    const fixedUID = addAndMaybeUpdateUID(value.uid)
    return {
      ...value,
      uid: fixedUID,
    }
  }

  function fixJSNestedArray(value: JSExpressionNestedArray): JSExpressionNestedArray {
    const fixedUID = addAndMaybeUpdateUID(value.uid)
    return {
      ...value,
      content: value.content.map(fixJSXArrayElement),
      uid: fixedUID,
    }
  }

  function fixJSNestedObject(value: JSExpressionNestedObject): JSExpressionNestedObject {
    const fixedUID = addAndMaybeUpdateUID(value.uid)
    return {
      ...value,
      content: value.content.map(fixJSXProperty),
      uid: fixedUID,
    }
  }

  function fixUtopiaElementInner(element: JSXElementChild): JSXElementChild {
    switch (element.type) {
      case 'JSX_ELEMENT':
        return fixJSXElement(element)
      case 'JSX_FRAGMENT':
        return fixJSXFragment(element)
      case 'JSX_CONDITIONAL_EXPRESSION':
        return fixJSXConditionalExpression(element)
      case 'JSX_TEXT_BLOCK':
        return fixJSXTextBlock(element)
      case 'ATTRIBUTE_VALUE':
      case 'ATTRIBUTE_NESTED_ARRAY':
      case 'ATTRIBUTE_NESTED_OBJECT':
      case 'ATTRIBUTE_FUNCTION_CALL':
      case 'JSX_MAP_EXPRESSION':
      case 'ATTRIBUTE_OTHER_JAVASCRIPT':
      case 'JS_IDENTIFIER':
      case 'JS_PROPERTY_ACCESS':
      case 'JS_ELEMENT_ACCESS':
        return fixJSExpression(element)
      default:
        assertNever(element)
    }
  }

  const fixedValue = fixUtopiaElementInner(elementToFix)
  return {
    value: fixedValue,
    mappings: mappings,
  }
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
      element.originalJavascript,
      element.javascriptWithUIDs,
      element.transpiledJavascript,
      element.definedElsewhere,
      element.sourceMap,
      element.elementsWithin,
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
