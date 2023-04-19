import type {
  ArbitraryJSBlock,
  ElementsWithin,
  JSExpression,
  JSExpressionFunctionCall,
  JSExpressionNestedArray,
  JSExpressionNestedObject,
  JSExpressionOtherJavaScript,
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
import { fromField } from '../../../core/shared/optics/optic-creators'
import { assertNever } from '../../../core/shared/utils'

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

interface FixUIDsState {
  mutableAllNewUIDs: Set<string>
  mappings: Array<{ oldUID: string; newUID: string }>
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
    oldParsed.combinedTopLevelArbitraryBlock == null ||
    newParsed.combinedTopLevelArbitraryBlock == null
      ? newParsed.combinedTopLevelArbitraryBlock
      : fixArbitraryJSBlockUIDs(
          oldParsed.combinedTopLevelArbitraryBlock,
          newParsed.combinedTopLevelArbitraryBlock,
          fixUIDsState,
        )

  // This needs to be corrected as things may have moved around.
  const fixedHighlightBounds: HighlightBoundsForUids = {
    ...newParsed.highlightBounds,
  }
  for (const { oldUID, newUID } of fixUIDsState.mappings) {
    // Protect against highlight bounds not being defined for this case.
    if (oldUID in fixedHighlightBounds) {
      const bounds = fixedHighlightBounds[oldUID]
      delete fixedHighlightBounds[oldUID]
      fixedHighlightBounds[newUID] = bounds
    }
  }

  // Return the result.
  return {
    ...newParsed,
    topLevelElements: fixedTopLevelElements,
    combinedTopLevelArbitraryBlock: fixedCombinedTopLevelArbitraryBlock,
    highlightBounds: fixedHighlightBounds,
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
    // The old uid is already present somewhere, so using it will introduce a duplicate.
    uidToUse = newUID
  } else {
    // The uids have changed:
    // - Remove the new one from the set, as now it will be unused.
    // - Add the old one to the set, as it will become used.
    // - Return the old one.
    // One assumption here is that there is no duplicate uids in the new elements.
    fixUIDsState.mutableAllNewUIDs.delete(newUID)
    fixUIDsState.mutableAllNewUIDs.add(oldUID)
    fixUIDsState.mappings.push({ oldUID: oldUID, newUID: newUID })
    uidToUse = oldUID
  }

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
  fixElement: (oldElement: T, newElement: T) => T,
  oldExpression: Array<T>,
  newExpression: Array<T>,
): Array<T> {
  return newExpression.map((newElement, newElementIndex) => {
    if (newElementIndex < oldExpression.length) {
      const oldElement = oldExpression[newElementIndex]
      return fixElement(oldElement, newElement)
    } else {
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
    (oldElement, newElement) => {
      return fixTopLevelElementUIDs(oldElement, newElement, fixUIDsState)
    },
    oldElements,
    newElements,
  )
}

export function fixTopLevelElementUIDs(
  oldElement: TopLevelElement,
  newElement: TopLevelElement,
  fixUIDsState: FixUIDsState,
): TopLevelElement {
  switch (newElement.type) {
    case 'UTOPIA_JSX_COMPONENT': {
      if (oldElement.type === newElement.type) {
        return fixUtopiaJSXComponentUIDs(oldElement, newElement, fixUIDsState)
      }
      break
    }
    case 'ARBITRARY_JS_BLOCK': {
      if (oldElement.type === newElement.type) {
        return fixArbitraryJSBlockUIDs(oldElement, newElement, fixUIDsState)
      }
      break
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

  return newElement
}

export function fixUtopiaJSXComponentUIDs(
  oldElement: UtopiaJSXComponent,
  newElement: UtopiaJSXComponent,
  fixUIDsState: FixUIDsState,
): UtopiaJSXComponent {
  const fixedArbitraryJSBlock =
    oldElement.arbitraryJSBlock == null || newElement.arbitraryJSBlock == null
      ? newElement.arbitraryJSBlock
      : fixArbitraryJSBlockUIDs(
          oldElement.arbitraryJSBlock,
          newElement.arbitraryJSBlock,
          fixUIDsState,
        )
  const fixedRootElement = fixJSXElementChildUIDs(
    oldElement.rootElement,
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
  oldElement: ArbitraryJSBlock,
  newElement: ArbitraryJSBlock,
  fixUIDsState: FixUIDsState,
): ArbitraryJSBlock {
  const fixedElementsWithin = fixElementsWithin(
    oldElement.elementsWithin,
    newElement.elementsWithin,
    fixUIDsState,
  )
  return {
    ...newElement,
    elementsWithin: fixedElementsWithin,
  }
}

export function fixJSXArrayElement(
  oldElement: JSXArrayElement,
  newElement: JSXArrayElement,
  fixUIDsState: FixUIDsState,
): JSXArrayElement {
  return {
    ...newElement,
    value: fixExpressionUIDs(oldElement.value, newElement.value, fixUIDsState),
  }
}

export function fixJSXArrayElements(
  oldExpression: Array<JSXArrayElement>,
  newExpression: Array<JSXArrayElement>,
  fixUIDsState: FixUIDsState,
): Array<JSXArrayElement> {
  return fixArrayElements(
    (oldElement, newElement) => {
      return fixJSXArrayElement(oldElement, newElement, fixUIDsState)
    },
    oldExpression,
    newExpression,
  )
}

export function fixJSXProperty(
  oldExpression: JSXProperty,
  newExpression: JSXProperty,
  fixUIDsState: FixUIDsState,
): JSXProperty {
  return {
    ...newExpression,
    value: fixExpressionUIDs(oldExpression.value, newExpression.value, fixUIDsState),
  }
}

export function fixJSXPropertyArray(
  oldExpression: Array<JSXProperty>,
  newExpression: Array<JSXProperty>,
  fixUIDsState: FixUIDsState,
): Array<JSXProperty> {
  return fixArrayElements(
    (oldElement, newElement) => {
      return fixJSXProperty(oldElement, newElement, fixUIDsState)
    },
    oldExpression,
    newExpression,
  )
}

export function fixExpressionArray(
  oldExpression: Array<JSExpression>,
  newExpression: Array<JSExpression>,
  fixUIDsState: FixUIDsState,
): Array<JSExpression> {
  return fixArrayElements(
    (oldElement, newElement) => {
      return fixExpressionUIDs(oldElement, newElement, fixUIDsState)
    },
    oldExpression,
    newExpression,
  )
}

export function fixJSXAttributesPart(
  oldExpression: JSXAttributesPart,
  newExpression: JSXAttributesPart,
  fixUIDsState: FixUIDsState,
): JSXAttributesPart {
  switch (newExpression.type) {
    case 'JSX_ATTRIBUTES_ENTRY': {
      if (oldExpression.type === newExpression.type) {
        const fixedValue = fixExpressionUIDs(oldExpression.value, newExpression.value, fixUIDsState)
        return {
          ...newExpression,
          value: fixedValue,
        }
      }
      break
    }
    case 'JSX_ATTRIBUTES_SPREAD': {
      if (oldExpression.type === newExpression.type) {
        const fixedSpreadValue = fixExpressionUIDs(
          oldExpression.spreadValue,
          newExpression.spreadValue,
          fixUIDsState,
        )
        return {
          ...newExpression,
          spreadValue: fixedSpreadValue,
        }
      }
      break
    }
    default:
      assertNever(newExpression)
  }

  return newExpression
}

export function fixJSXAttributesUIDs(
  oldExpression: JSXAttributes,
  newExpression: JSXAttributes,
  fixUIDsState: FixUIDsState,
): JSXAttributes {
  return fixArrayElements(
    (oldPart, newPart) => {
      return fixJSXAttributesPart(oldPart, newPart, fixUIDsState)
    },
    oldExpression,
    newExpression,
  )
}

export function fixJSXElementChildArray(
  oldElements: Array<JSXElementChild>,
  newElements: Array<JSXElementChild>,
  fixUIDsState: FixUIDsState,
): Array<JSXElementChild> {
  return fixArrayElements(
    (oldElement, newElement) => {
      return fixJSXElementChildUIDs(oldElement, newElement, fixUIDsState)
    },
    oldElements,
    newElements,
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
    if (newWithinKey in oldExpression) {
      const oldElement = newExpression[newWithinKey]
      const fixedElement = fixJSXElementUIDs(oldElement, newElement, fixUIDsState)
      result[fixedElement.uid] = fixedElement
    } else {
      result[newWithinKey] = newElement
    }
  }

  return result
}

export function fixJSXElementChildUIDs(
  oldElement: JSXElementChild,
  newElement: JSXElementChild,
  fixUIDsState: FixUIDsState,
): JSXElementChild {
  switch (newElement.type) {
    case 'JSX_ELEMENT': {
      if (oldElement.type === newElement.type) {
        return fixJSXElementUIDs(oldElement, newElement, fixUIDsState)
      }
      break
    }
    case 'JSX_FRAGMENT': {
      if (oldElement.type === newElement.type) {
        const updatedChildren = fixJSXElementChildArray(
          oldElement.children,
          newElement.children,
          fixUIDsState,
        )
        return updateUID(jsxFragmentUIDOptic, oldElement.uid, fixUIDsState, {
          ...newElement,
          children: updatedChildren,
        })
      }
      break
    }
    case 'JSX_TEXT_BLOCK': {
      if (oldElement.type === newElement.type) {
        return updateUID(jsxTextBlockUIDOptic, oldElement.uid, fixUIDsState, newElement)
      }
      break
    }
    case 'JSX_CONDITIONAL_EXPRESSION': {
      if (oldElement.type === newElement.type) {
        const updatedCondition = fixExpressionUIDs(
          oldElement.condition,
          newElement.condition,
          fixUIDsState,
        )
        const updatedWhenTrue = fixJSXElementChildUIDs(
          oldElement.whenTrue,
          newElement.whenTrue,
          fixUIDsState,
        )
        const updatedWhenFalse = fixJSXElementChildUIDs(
          oldElement.whenFalse,
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
      break
    }
    case 'ATTRIBUTE_VALUE':
    case 'ATTRIBUTE_NESTED_ARRAY':
    case 'ATTRIBUTE_NESTED_OBJECT':
    case 'ATTRIBUTE_FUNCTION_CALL':
    case 'ATTRIBUTE_OTHER_JAVASCRIPT': {
      if (oldElement.type === newElement.type) {
        return fixExpressionUIDs(oldElement, newElement, fixUIDsState)
      }
      break
    }
    default:
      assertNever(newElement)
  }

  return newElement
}

export function fixJSXElementUIDs(
  oldElement: JSXElement,
  newElement: JSXElement,
  fixUIDsState: FixUIDsState,
): JSXElement {
  const fixedProps = fixJSXAttributesUIDs(oldElement.props, newElement.props, fixUIDsState)
  const fixedChildren = fixJSXElementChildArray(
    oldElement.children,
    newElement.children,
    fixUIDsState,
  )
  return updateUID(jsxElementUIDOptic, oldElement.uid, fixUIDsState, {
    ...newElement,
    props: fixedProps,
    children: fixedChildren,
  })
}

export function fixExpressionUIDs(
  oldExpression: JSExpression,
  newExpression: JSExpression,
  fixUIDsState: FixUIDsState,
): JSExpression {
  switch (newExpression.type) {
    case 'ATTRIBUTE_VALUE': {
      if (oldExpression.type === newExpression.type) {
        return updateUID(expressionValueUIDOptic, oldExpression.uid, fixUIDsState, newExpression)
      }
      break
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
      }
      break
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
      }
      break
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
      }
      break
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
      }
      break
    }
    default:
      assertNever(newExpression)
  }

  return newExpression
}
