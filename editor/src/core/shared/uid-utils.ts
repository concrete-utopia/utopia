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
} from './element-template'
import { shallowEqual } from './equality-utils'
import {
  getModifiableJSXAttributeAtPath,
  jsxSimpleAttributeToValue,
  setJSXValueAtPath,
} from './jsx-attributes'
import * as PP from './property-path'
import * as TP from './template-path'
import { objectMap, objectValues } from './object-utils'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'
import { getDOMAttribute } from './dom-utils'
import { UTOPIA_PATHS_KEY, UTOPIA_UIDS_KEY } from '../model/utopia-constants'
import { optionalMap } from './optional-utils'
import { addAllUniquely } from './array-utils'
import { InstancePath } from './project-file-types'

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
  return {
    ...element,
    props: setJSXAttributesAttribute(
      element.props,
      'data-uid',
      jsxAttributeValue(uid, emptyComments),
    ),
  }
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
  const possibleUID = parseUID(element.props)
  if (isLeft(possibleUID)) {
    throw new Error('Every Utopia Element must have a valid props.data-uid')
  } else {
    return possibleUID.value
  }
}

export function fixUtopiaElement(
  elementToFix: JSXElementChild,
  uniqueIDs: Array<string>,
): JSXElementChild {
  function fixUtopiaElementInner<T extends JSXElementChild>(element: T): T {
    if (isJSXElement(element)) {
      let fixedChildren = element.children.map((elem) => fixUtopiaElementInner(elem))
      if (shallowEqual(element.children, fixedChildren)) {
        // saving reference equality in case the children didn't need fixing
        fixedChildren = element.children
      }

      const uidProp = getJSXAttribute(element.props, 'data-uid')
      if (uidProp == null || !isJSXAttributeValue(uidProp) || uniqueIDs.includes(uidProp.value)) {
        const newUID = generateUID(uniqueIDs)
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
          return {
            ...element,
            props: fixedProps.value,
            children: fixedChildren,
          }
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
    } else if (isJSXArbitraryBlock(element)) {
      const fixedElementsWithin = objectMap(fixUtopiaElementInner, element.elementsWithin)
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

export function uidsFromString(uidList: string = ''): Array<string> {
  return uidList.split(' ')
}

export function uidsToString(uidList: Array<string>): string {
  return uidList.join(' ')
}

export function popFrontUID(
  uidList: string | null | undefined,
): { head: string | null; tail: string | null } {
  if (uidList == null) {
    return { head: null, tail: null }
  }
  const uids = uidsFromString(uidList)
  const head = uids[0]
  const tail = uids.slice(1)
  return {
    head: uids[0],
    tail: tail.length > 0 ? uidsToString(tail) : null,
  }
}

export function appendToUidString(
  uidsString: string | null | undefined,
  uidsToAppendString: string | null | undefined,
): string | null {
  if (uidsToAppendString == null) {
    return uidsString ?? null
  } else if (uidsString == null || uidsString.length === 0) {
    return uidsToAppendString
  } else {
    const existingUIDs = uidsFromString(uidsString)
    const uidsToAppend = uidsFromString(uidsToAppendString)
    const updatedUIDs = addAllUniquely(existingUIDs, uidsToAppend)
    return uidsToString(updatedUIDs)
  }
}

export function getPathsOnDomElement(element: Element): Array<InstancePath> {
  const pathsAttribute = getDOMAttribute(element, UTOPIA_PATHS_KEY)
  return (
    optionalMap((pathsString: string) => {
      return pathsString.split(' ').map(TP.fromString).filter(TP.isInstancePath)
    }, pathsAttribute) ?? []
  )
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
        for (const elementWithin of objectValues(element.elementsWithin)) {
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
    const parsedUID = parseUID(element.props)
    if (isRight(parsedUID) && parsedUID.value === targetUID) {
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
