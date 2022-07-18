import { v4 as UUID } from 'uuid'
import { Either, flatMapEither, isLeft, isRight, left, right } from './either'
import {
  JSXAttributes,
  jsxAttributeValue,
  JSXElement,
  JSXElementChild,
  isJSXElement,
  isJSXAttributeValue,
  isJSXArbitraryBlock,
  setJSXAttributesAttribute,
  getJSXAttribute,
  TopLevelElement,
  jsxElement,
  emptyComments,
} from './element-template'
import { shallowEqual } from './equality-utils'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
} from './jsx-attributes'
import * as PP from './property-path'
import * as EP from './element-path'
import { objectMap, objectValues } from './object-utils'
import { getDOMAttribute } from './dom-utils'
import { UTOPIA_PATH_KEY, UTOPIA_UID_KEY } from '../model/utopia-constants'
import { addAllUniquely, mapDropNulls } from './array-utils'
import { ElementPath } from './project-file-types'

export const UtopiaIDPropertyPath = PP.create(['data-uid'])

const atoz = [
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
  existingIDs: Set<string>,
  possibleStartingValue: string,
): string {
  if (possibleStartingValue.length >= 3) {
    const maxSteps = Math.floor(possibleStartingValue.length / 3)
    for (let step = 0; step < maxSteps; step++) {
      const possibleUID = possibleStartingValue.substring(step * 3, (step + 1) * 3)

      if (!existingIDs.has(possibleUID)) {
        return possibleUID
      }
    }

    for (let firstChar of atoz) {
      for (let secondChar of atoz) {
        for (let thirdChar of atoz) {
          const possibleUID = `${firstChar}${secondChar}${thirdChar}`

          if (!existingIDs.has(possibleUID)) {
            return possibleUID
          }
        }
      }
    }
  }

  // Fallback bailout.
  throw new Error(`Unable to generate a UID from ${possibleStartingValue}`)
}

export function generateUID(existingIDs: Array<string> | Set<string>): string {
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
}

export const GeneratedUIDSeparator = `~~~`
export function createIndexedUid(originalUid: string, index: string | number): string {
  return `${originalUid}${GeneratedUIDSeparator}${index}`
}

export function extractOriginalUidFromIndexedUid(uid: string): string {
  const separatorIndex = uid.indexOf(GeneratedUIDSeparator)
  if (separatorIndex >= 0) {
    return uid.substr(0, separatorIndex)
  } else {
    return uid
  }
}

export function setUtopiaIDOnJSXElement(element: JSXElement, uid: string): JSXElement {
  return jsxElement(
    element.name,
    uid,
    setJSXAttributesAttribute(element.props, 'data-uid', jsxAttributeValue(uid, emptyComments)),
    element.children,
  )
}

export function parseUID(attributes: JSXAttributes): Either<string, string> {
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

export function getUtopiaIDFromJSXElement(element: JSXElement): string {
  return element.uid
}

export function fixUtopiaElement(
  elementToFix: JSXElementChild,
  uniqueIDs: Array<string>,
): JSXElementChild {
  function fixJSXElement(element: JSXElement): JSXElement {
    let fixedChildren = element.children.map((elem) => fixUtopiaElementInner(elem))
    if (shallowEqual(element.children, fixedChildren)) {
      // saving reference equality in case the children didn't need fixing
      fixedChildren = element.children
    }

    const uidProp = getJSXAttribute(element.props, 'data-uid')
    if (uidProp == null || !isJSXAttributeValue(uidProp) || uniqueIDs.includes(uidProp.value)) {
      const seedUID = uidProp != null && isJSXAttributeValue(uidProp) ? uidProp.value : 'aaa'
      const newUID = generateConsistentUID(new Set(uniqueIDs), seedUID)
      const fixedProps = setJSXValueAtPath(
        element.props,
        UtopiaIDPropertyPath,
        jsxAttributeValue(newUID, emptyComments),
      )

      if (isLeft(fixedProps)) {
        console.error(`Failed to add a uid to an element missing one ${fixedProps.value}`)
        return element
      } else {
        uniqueIDs.push(newUID)
        return jsxElement(element.name, newUID, fixedProps.value, fixedChildren)
      }
    } else if (element.children !== fixedChildren) {
      uniqueIDs.push(uidProp.value)
      return {
        ...element,
        children: fixedChildren,
      }
    } else {
      uniqueIDs.push(uidProp.value)
      return element
    }
  }

  function fixUtopiaElementInner(element: JSXElementChild): JSXElementChild {
    if (isJSXElement(element)) {
      return fixJSXElement(element)
    } else if (isJSXArbitraryBlock(element)) {
      const fixedElementsWithin = objectMap(fixJSXElement, element.elementsWithin)
      if (shallowEqual(element.elementsWithin, fixedElementsWithin)) {
        return element
      } else {
        return {
          ...element,
          elementsWithin: fixedElementsWithin,
        }
      }
    } else {
      return element
    }
  }

  return fixUtopiaElementInner(elementToFix)
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
      case 'JSX_ARBITRARY_BLOCK':
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
