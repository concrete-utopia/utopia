import {
  ArbitraryJSBlock,
  ElementsWithin,
  emptyComments,
  getJSXAttribute,
  isJSExpressionValue,
  isJSXElement,
  JSExpression,
  JSExpressionFunctionCall,
  JSExpressionNestedArray,
  JSExpressionNestedObject,
  JSExpressionOtherJavaScript,
  jsExpressionValue,
  JSExpressionValue,
  JSXArrayElement,
  JSXAttributes,
  JSXAttributesPart,
  JSXConditionalExpression,
  JSXElement,
  JSXElementChild,
  JSXFragment,
  JSXProperty,
  JSXTextBlock,
  setJSXAttributesAttribute,
  TopLevelElement,
  UtopiaJSXComponent,
} from '../../shared/element-template'
import {
  ParsedTextFile,
  ParseSuccess,
  HighlightBoundsForUids,
  isParseSuccess,
} from '../../shared/project-file-types'
import type { Optic } from '../../../core/shared/optics/optics'
import { set, unsafeGet } from '../../../core/shared/optics/optic-utilities'
import { fromField, identityOptic } from '../../../core/shared/optics/optic-creators'
import { assertNever } from '../../../core/shared/utils'
import { emptySet } from '../../../core/shared/set-utils'
import {
  generateConsistentUID,
  UIDMappings,
  updateHighlightBounds,
} from '../../../core/shared/uid-utils'

const jsxElementChildUIDOptic: Optic<JSXElementChild, string> = fromField('uid')

const jsxElementUIDOptic: Optic<JSXElement, string> = fromField('uid')

const jsxFragmentUIDOptic: Optic<JSXFragment, string> = fromField('uid')

const jsxTextBlockUIDOptic: Optic<JSXTextBlock, string> = fromField('uid')

const jsxConditionalExpressionUIDOptic: Optic<JSXConditionalExpression, string> = fromField('uid')

const expressionValueUIDOptic: Optic<JSExpressionValue<any>, string> = fromField('uid')

const expressionNestedArrayUIDOptic: Optic<JSExpressionNestedArray, string> = fromField('uid')

const expressionNestedObjectUIDOptic: Optic<JSExpressionNestedObject, string> = fromField('uid')

const expressionFunctionCallUIDOptic: Optic<JSExpressionFunctionCall, string> = fromField('uid')

const expressionOtherJavaScriptUIDOptic: Optic<JSExpressionOtherJavaScript, string> =
  fromField('uid')

const jsExpressionUIDOptic: Optic<JSExpression, string> = fromField('uid')

interface FixUIDsState {
  mutableAllNewUIDs: Set<string>
  mappings: UIDMappings
}

export function fixParseSuccessUIDs(
  oldParsed: ParseSuccess | null,
  newParsed: ParsedTextFile,
  alreadyExistingUIDs: Set<string>,
): ParsedTextFile {
  if (oldParsed == null || !isParseSuccess(newParsed)) {
    // We won't try to fix parse failures.
    return newParsed
  }

  // This gets passed through all the fixing functions, so
  // that the changes can be recorded.
  const fixUIDsState: FixUIDsState = {
    mutableAllNewUIDs: alreadyExistingUIDs,
    mappings: [],
  }

  // Fix the UIDs in the content.
  const fixedTopLevelElements = fixTopLevelElementsUIDs(
    oldParsed.topLevelElements,
    newParsed.topLevelElements,
    fixUIDsState,
  )
  const fixedCombinedTopLevelArbitraryBlock =
    newParsed.combinedTopLevelArbitraryBlock == null
      ? newParsed.combinedTopLevelArbitraryBlock
      : fixArbitraryJSBlockUIDs(
          oldParsed.combinedTopLevelArbitraryBlock,
          newParsed.combinedTopLevelArbitraryBlock,
          fixUIDsState,
        )

  // This needs to be corrected as things may have moved around.
  const fixedHighlightBounds: HighlightBoundsForUids = updateHighlightBounds(
    newParsed.highlightBounds,
    fixUIDsState.mappings,
  )
  const fixedFullHighlightBounds: HighlightBoundsForUids = updateHighlightBounds(
    newParsed.fullHighlightBounds,
    fixUIDsState.mappings,
  )

  // Return the result.
  return {
    ...newParsed,
    topLevelElements: fixedTopLevelElements,
    combinedTopLevelArbitraryBlock: fixedCombinedTopLevelArbitraryBlock,
    highlightBounds: fixedHighlightBounds,
    fullHighlightBounds: fixedFullHighlightBounds,
  }
}

function updateUID<T>(
  uidOptic: Optic<T, string>,
  oldUID: string,
  fixUIDsState: FixUIDsState,
  baseValue: T,
): T {
  const newUID = unsafeGet(uidOptic, baseValue)
  let uidToUse: string
  if (oldUID === newUID) {
    // Old one is the same as the new one, so everything is great.
    uidToUse = newUID
  } else if (fixUIDsState.mutableAllNewUIDs.has(oldUID)) {
    // The UID has changed, but the UID is already used elsewhere in the new structure:
    // - Generate a new consistent UID.
    // - Add the old one to the set, as it will become used.
    // - Add a mapping for this change.
    uidToUse = generateConsistentUID(fixUIDsState.mutableAllNewUIDs, oldUID)
    fixUIDsState.mappings.push({ originalUID: newUID, newUID: uidToUse })
  } else {
    // The UID has changed, add a mapping so the highlight bounds can be updated.
    uidToUse = oldUID
    fixUIDsState.mappings.push({ originalUID: newUID, newUID: uidToUse })
  }
  fixUIDsState.mutableAllNewUIDs.add(uidToUse)

  if (newUID === uidToUse) {
    // As there's no change, don't create a new object.
    return baseValue
  } else {
    // In this case the uid needs updating.
    return set(uidOptic, uidToUse, baseValue)
  }
}

// Should we find the need for some smarter handling of arrays of things in the future.
// Like for example handling something being inserted in the middle and shifting the uids appropriately.
// Then improving this function will make all of the cases where it is used better.
function fixArrayElements<T>(
  uidOptic: Optic<T, string> | null,
  fixElement: (oldElement: T | null | undefined, newElement: T) => T,
  oldExpression: Array<T> | null | undefined,
  newExpression: Array<T>,
  fixUIDsState: FixUIDsState,
): Array<T> {
  // Should an optic be available for looking up UIDs for these entries, then check for entries at a
  // different index in the array with the same UID.
  // If this is the case it's likely to be indicative of a new entry being inserted into the array,
  // which has shifted the positions.
  let oldElementIndexesUsed: Set<number> = emptySet()
  let workingArray: Array<T> = []
  if (uidOptic != null) {
    newExpression.forEach((newElement, newElementIndex) => {
      const uidOfNewElement = unsafeGet(uidOptic, newElement)
      let possibleOldElement: T | undefined = undefined
      if (oldExpression != null) {
        oldExpression.forEach((oldElement, oldElementIndex) => {
          if (unsafeGet(uidOptic, oldElement) === uidOfNewElement) {
            possibleOldElement = oldElement
            oldElementIndexesUsed.add(oldElementIndex)
          }
        })
        if (possibleOldElement != null) {
          workingArray[newElementIndex] = fixElement(possibleOldElement, newElement)
        }
      }
    })
  }

  return newExpression.map((newElement, newElementIndex) => {
    if (newElementIndex in workingArray) {
      // Look for those shifted values.
      return workingArray[newElementIndex]
    } else if (!oldElementIndexesUsed.has(newElementIndex)) {
      // If this entry hasn't been allocated to another index.
      const oldElement = oldExpression?.[newElementIndex]
      return fixElement(oldElement, newElement)
    } else {
      // Fallback case.
      return newElement
    }
  })
}

export function fixTopLevelElementsUIDs(
  oldElements: Array<TopLevelElement>,
  newElements: Array<TopLevelElement>,
  fixUIDsState: FixUIDsState,
): Array<TopLevelElement> {
  return fixArrayElements(
    null,
    (oldElement, newElement) => {
      return fixTopLevelElementUIDs(oldElement, newElement, fixUIDsState)
    },
    oldElements,
    newElements,
    fixUIDsState,
  )
}

export function fixTopLevelElementUIDs(
  oldElement: TopLevelElement | null | undefined,
  newElement: TopLevelElement,
  fixUIDsState: FixUIDsState,
): TopLevelElement {
  switch (newElement.type) {
    case 'UTOPIA_JSX_COMPONENT': {
      return fixUtopiaJSXComponentUIDs(
        oldElement?.type === newElement.type ? oldElement : null,
        newElement,
        fixUIDsState,
      )
    }
    case 'ARBITRARY_JS_BLOCK': {
      return fixArbitraryJSBlockUIDs(
        oldElement?.type === newElement.type ? oldElement : null,
        newElement,
        fixUIDsState,
      )
    }
    case 'IMPORT_STATEMENT': {
      return newElement
    }
    case 'UNPARSED_CODE': {
      return newElement
    }
    default:
      assertNever(newElement)
  }
}

export function fixUtopiaJSXComponentUIDs(
  oldElement: UtopiaJSXComponent | null | undefined,
  newElement: UtopiaJSXComponent,
  fixUIDsState: FixUIDsState,
): UtopiaJSXComponent {
  const fixedArbitraryJSBlock =
    newElement.arbitraryJSBlock == null
      ? newElement.arbitraryJSBlock
      : fixArbitraryJSBlockUIDs(
          oldElement?.arbitraryJSBlock,
          newElement.arbitraryJSBlock,
          fixUIDsState,
        )
  const fixedRootElement = fixJSXElementChildUIDs(
    oldElement?.rootElement,
    newElement.rootElement,
    fixUIDsState,
  )
  return {
    ...newElement,
    arbitraryJSBlock: fixedArbitraryJSBlock,
    rootElement: fixedRootElement,
  }
}

export function fixArbitraryJSBlockUIDs(
  oldElement: ArbitraryJSBlock | null | undefined,
  newElement: ArbitraryJSBlock,
  fixUIDsState: FixUIDsState,
): ArbitraryJSBlock {
  const fixedElementsWithin = fixElementsWithin(
    oldElement?.elementsWithin ?? {},
    newElement.elementsWithin,
    fixUIDsState,
  )
  return {
    ...newElement,
    elementsWithin: fixedElementsWithin,
  }
}

export function fixJSXArrayElement(
  oldElement: JSXArrayElement | null | undefined,
  newElement: JSXArrayElement,
  fixUIDsState: FixUIDsState,
): JSXArrayElement {
  return {
    ...newElement,
    value: fixExpressionUIDs(oldElement?.value, newElement.value, fixUIDsState),
  }
}

export function fixJSXArrayElements(
  oldExpression: Array<JSXArrayElement>,
  newExpression: Array<JSXArrayElement>,
  fixUIDsState: FixUIDsState,
): Array<JSXArrayElement> {
  return fixArrayElements(
    null,
    (oldElement, newElement) => {
      return fixJSXArrayElement(oldElement, newElement, fixUIDsState)
    },
    oldExpression,
    newExpression,
    fixUIDsState,
  )
}

export function fixJSXProperty(
  oldExpression: JSXProperty | null | undefined,
  newExpression: JSXProperty,
  fixUIDsState: FixUIDsState,
): JSXProperty {
  return {
    ...newExpression,
    value: fixExpressionUIDs(oldExpression?.value, newExpression.value, fixUIDsState),
  }
}

export function fixJSXPropertyArray(
  oldExpression: Array<JSXProperty>,
  newExpression: Array<JSXProperty>,
  fixUIDsState: FixUIDsState,
): Array<JSXProperty> {
  return fixArrayElements(
    null,
    (oldElement, newElement) => {
      return fixJSXProperty(oldElement, newElement, fixUIDsState)
    },
    oldExpression,
    newExpression,
    fixUIDsState,
  )
}

export function fixExpressionArray(
  oldExpression: Array<JSExpression>,
  newExpression: Array<JSExpression>,
  fixUIDsState: FixUIDsState,
): Array<JSExpression> {
  return fixArrayElements(
    jsExpressionUIDOptic,
    (oldElement, newElement) => {
      return fixExpressionUIDs(oldElement, newElement, fixUIDsState)
    },
    oldExpression,
    newExpression,
    fixUIDsState,
  )
}

export function fixJSXAttributesPart(
  oldExpression: JSXAttributesPart | null | undefined,
  newExpression: JSXAttributesPart,
  fixUIDsState: FixUIDsState,
): JSXAttributesPart {
  switch (newExpression.type) {
    case 'JSX_ATTRIBUTES_ENTRY': {
      const fixedValue = fixExpressionUIDs(
        oldExpression?.type === newExpression.type ? oldExpression.value : null,
        newExpression.value,
        fixUIDsState,
      )
      return {
        ...newExpression,
        value: fixedValue,
      }
    }
    case 'JSX_ATTRIBUTES_SPREAD': {
      const fixedSpreadValue = fixExpressionUIDs(
        oldExpression?.type === newExpression.type ? oldExpression.spreadValue : null,
        newExpression.spreadValue,
        fixUIDsState,
      )
      return {
        ...newExpression,
        spreadValue: fixedSpreadValue,
      }
    }
    default:
      assertNever(newExpression)
  }
}

export function fixJSXAttributesUIDs(
  oldExpression: JSXAttributes | null | undefined,
  newExpression: JSXAttributes,
  fixUIDsState: FixUIDsState,
): JSXAttributes {
  return fixArrayElements(
    null,
    (oldPart, newPart) => {
      return fixJSXAttributesPart(oldPart, newPart, fixUIDsState)
    },
    oldExpression,
    newExpression,
    fixUIDsState,
  )
}

export function fixJSXElementChildArray(
  oldElements: Array<JSXElementChild>,
  newElements: Array<JSXElementChild>,
  fixUIDsState: FixUIDsState,
): Array<JSXElementChild> {
  return fixArrayElements(
    jsxElementChildUIDOptic,
    (oldElement, newElement) => {
      return fixJSXElementChildUIDs(oldElement, newElement, fixUIDsState)
    },
    oldElements,
    newElements,
    fixUIDsState,
  )
}

export function fixElementsWithin(
  oldExpression: ElementsWithin,
  newExpression: ElementsWithin,
  fixUIDsState: FixUIDsState,
): ElementsWithin {
  let result: ElementsWithin = {}
  for (const newWithinKey of Object.keys(newExpression)) {
    const newElement = newExpression[newWithinKey]
    const oldElement = oldExpression[newWithinKey]
    const fixedElement = fixJSXElementUIDs(oldElement, newElement, fixUIDsState)
    result[fixedElement.uid] = fixedElement
  }

  return result
}

export function fixJSXElementChildUIDs(
  oldElement: JSXElementChild | null | undefined,
  newElement: JSXElementChild,
  fixUIDsState: FixUIDsState,
): JSXElementChild {
  switch (newElement.type) {
    case 'JSX_ELEMENT': {
      return fixJSXElementUIDs(oldElement, newElement, fixUIDsState)
    }
    case 'JSX_FRAGMENT': {
      if (oldElement == null) {
        return newElement
      } else if (oldElement == null || oldElement.type === newElement.type) {
        const updatedChildren = fixJSXElementChildArray(
          oldElement.children,
          newElement.children,
          fixUIDsState,
        )
        return updateUID(jsxFragmentUIDOptic, oldElement.uid, fixUIDsState, {
          ...newElement,
          children: updatedChildren,
        })
      } else {
        return updateUID(jsxFragmentUIDOptic, oldElement.uid, fixUIDsState, newElement)
      }
    }
    case 'JSX_TEXT_BLOCK': {
      if (oldElement == null) {
        return newElement
      } else {
        return updateUID(jsxTextBlockUIDOptic, oldElement.uid, fixUIDsState, newElement)
      }
    }
    case 'JSX_CONDITIONAL_EXPRESSION': {
      if (oldElement == null) {
        return newElement
      } else {
        const updatedCondition = fixExpressionUIDs(
          oldElement.type === newElement.type ? oldElement.condition : null,
          newElement.condition,
          fixUIDsState,
        )
        const updatedWhenTrue = fixJSXElementChildUIDs(
          oldElement.type === newElement.type ? oldElement.whenTrue : null,
          newElement.whenTrue,
          fixUIDsState,
        )
        const updatedWhenFalse = fixJSXElementChildUIDs(
          oldElement.type === newElement.type ? oldElement.whenFalse : null,
          newElement.whenFalse,
          fixUIDsState,
        )
        return updateUID(jsxConditionalExpressionUIDOptic, oldElement.uid, fixUIDsState, {
          ...newElement,
          condition: updatedCondition,
          whenTrue: updatedWhenTrue,
          whenFalse: updatedWhenFalse,
        })
      }
    }
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT': {
      if (oldElement == null || oldElement.type === newElement.type) {
        return fixExpressionUIDs(oldElement, newElement, fixUIDsState)
      } else {
        return updateUID(jsExpressionUIDOptic, oldElement.uid, fixUIDsState, newElement)
      }
    }
    default:
      assertNever(newElement)
  }
}

export function fixJSXElementUIDs(
  oldElement: JSXElementChild | null | undefined,
  newElement: JSXElement,
  fixUIDsState: FixUIDsState,
): JSXElement {
  if (oldElement == null) {
    // No previous entry, so just return the new value.
    return newElement
  } else {
    // Update the UID upfront.
    const elementWithUpdatedUID = updateUID(
      jsxElementUIDOptic,
      oldElement.uid,
      fixUIDsState,
      newElement,
    )

    // Carry the UID of the prop that maybe set over as well.
    let dataUIDPropUID: string | undefined = undefined
    if (oldElement != null && isJSXElement(oldElement)) {
      const oldDataUIDProp = getJSXAttribute(oldElement.props, 'data-uid')
      const newDataUIDProp = getJSXAttribute(newElement.props, 'data-uid')
      if (oldDataUIDProp != null && newDataUIDProp != null) {
        dataUIDPropUID = updateUID(
          identityOptic<string>(),
          oldDataUIDProp.uid,
          fixUIDsState,
          newDataUIDProp.uid,
        )
      }
    }

    // Set the `data-uid` attribute.
    const attributesWithUpdatedUID: JSXAttributes = setJSXAttributesAttribute(
      elementWithUpdatedUID.props,
      'data-uid',
      jsExpressionValue(elementWithUpdatedUID.uid, emptyComments, dataUIDPropUID),
    )

    // If this is a `JSXElement`, then work through the common fields.
    let fixedProps: JSXAttributes = attributesWithUpdatedUID
    let fixedChildren: Array<JSXElementChild> = elementWithUpdatedUID.children
    if (isJSXElement(oldElement)) {
      fixedProps = fixJSXAttributesUIDs(oldElement.props, attributesWithUpdatedUID, fixUIDsState)
      fixedChildren = fixJSXElementChildArray(
        oldElement.children,
        elementWithUpdatedUID.children,
        fixUIDsState,
      )
    }

    return {
      ...elementWithUpdatedUID,
      props: fixedProps,
      children: fixedChildren,
    }
  }
}

export function fixExpressionUIDs(
  oldExpression: JSExpression | null | undefined,
  newExpression: JSExpression,
  fixUIDsState: FixUIDsState,
): JSExpression {
  if (oldExpression == null) {
    return newExpression
  } else {
    switch (newExpression.type) {
      case 'ATTRIBUTE_VALUE': {
        return updateUID(expressionValueUIDOptic, oldExpression.uid, fixUIDsState, newExpression)
      }
      case 'ATTRIBUTE_NESTED_ARRAY': {
        if (oldExpression.type === newExpression.type) {
          const fixedContents = fixJSXArrayElements(
            oldExpression.content,
            newExpression.content,
            fixUIDsState,
          )

          return updateUID(expressionNestedArrayUIDOptic, oldExpression.uid, fixUIDsState, {
            ...newExpression,
            content: fixedContents,
          })
        } else {
          return updateUID(
            expressionNestedArrayUIDOptic,
            oldExpression.uid,
            fixUIDsState,
            newExpression,
          )
        }
      }
      case 'ATTRIBUTE_NESTED_OBJECT': {
        if (oldExpression.type === newExpression.type) {
          const fixedContents = fixJSXPropertyArray(
            oldExpression.content,
            newExpression.content,
            fixUIDsState,
          )

          return updateUID(expressionNestedObjectUIDOptic, oldExpression.uid, fixUIDsState, {
            ...newExpression,
            content: fixedContents,
          })
        } else {
          return updateUID(
            expressionNestedObjectUIDOptic,
            oldExpression.uid,
            fixUIDsState,
            newExpression,
          )
        }
      }
      case 'ATTRIBUTE_FUNCTION_CALL': {
        if (oldExpression.type === newExpression.type) {
          const fixedParameters = fixExpressionArray(
            oldExpression.parameters,
            newExpression.parameters,
            fixUIDsState,
          )

          return updateUID(expressionFunctionCallUIDOptic, oldExpression.uid, fixUIDsState, {
            ...newExpression,
            parameters: fixedParameters,
          })
        } else {
          return updateUID(
            expressionFunctionCallUIDOptic,
            oldExpression.uid,
            fixUIDsState,
            newExpression,
          )
        }
      }
      case 'ATTRIBUTE_OTHER_JAVASCRIPT': {
        if (oldExpression.type === newExpression.type) {
          const fixedElementsWithin = fixElementsWithin(
            oldExpression.elementsWithin,
            newExpression.elementsWithin,
            fixUIDsState,
          )

          return updateUID(expressionOtherJavaScriptUIDOptic, oldExpression.uid, fixUIDsState, {
            ...newExpression,
            elementsWithin: fixedElementsWithin,
          })
        } else {
          return updateUID(
            expressionOtherJavaScriptUIDOptic,
            oldExpression.uid,
            fixUIDsState,
            newExpression,
          )
        }
      }
      default:
        assertNever(newExpression)
    }
  }
}
