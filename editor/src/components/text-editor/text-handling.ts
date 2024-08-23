import type { ElementPath } from '../../core/shared/project-file-types'
import type { AllElementProps, EditorState } from '../editor/store/editor-state'
import {
  getElementFromProjectContents,
  modifyUnderlyingTargetElement,
} from '../editor/store/editor-state'
import * as EP from '../../core/shared/element-path'
import type {
  ElementInstanceMetadataMap,
  JSExpression,
  JSXAttributes,
  JSXElementChild,
} from '../../core/shared/element-template'
import {
  getJSXElementNameAsString,
  isJSXElement,
  isJSXTextBlock,
  jsxTextBlock,
  isJSXAttributesEntry,
  isJSXConditionalExpression,
} from '../../core/shared/element-template'
import { jsxSimpleAttributeToValue } from '../../core/shared/jsx-attribute-utils'
import { foldEither } from '../../core/shared/either'
import fastDeepEquals from 'fast-deep-equal'
import { getUtopiaID } from '../../core/shared/uid-utils'
import type { ElementPathTrees } from '../../core/shared/element-path-tree'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { MaybeInfinityLocalRectangle } from '../../core/shared/math-utils'
import { isInfinityRectangle, localRectangle, nullIfInfinity } from '../../core/shared/math-utils'

// Validate this by making the type `Set<keyof CSSProperties>`.
export const stylePropertiesEligibleForMerge: Set<string> = new Set([
  'font',
  'fontFamily',
  'fontFeatureSettings',
  'fontKerning',
  'fontLanguageOverride',
  'fontOpticalSizing',
  'fontSize',
  'fontSizeAdjust',
  'fontStretch',
  'fontStyle',
  'fontSynthesis',
  'fontVariant',
  'fontVariantAlternates',
  'fontVariantCaps',
  'fontVariantEastAsian',
  'fontVariantLigatures',
  'fontVariantNumeric',
  'fontVariantPosition',
  'fontVariationSettings',
  'fontWeight',
])

function areStylePropsCompatible(
  targetStyleProp: JSExpression,
  toCheckStyleProp: JSExpression,
): boolean {
  const targetStyleValue = jsxSimpleAttributeToValue(targetStyleProp)
  return foldEither(
    () => {
      return false
    },
    (target) => {
      if (
        Object.keys(target).every((key) => {
          return stylePropertiesEligibleForMerge.has(key)
        })
      ) {
        const toCheckStyleValue = jsxSimpleAttributeToValue(toCheckStyleProp)
        return foldEither(
          () => {
            return false
          },
          (toCheck) => {
            if (
              Object.keys(toCheck).every((key) => {
                return stylePropertiesEligibleForMerge.has(key)
              })
            ) {
              // Check if the underlying values are identical.
              return fastDeepEquals(target, toCheck)
            } else {
              return false
            }
          },
          toCheckStyleValue,
        )
      } else {
        return false
      }
    },
    targetStyleValue,
  )
}

function arePropsCompatible(
  targetAttributes: JSXAttributes,
  toCheckAttributes: JSXAttributes,
): boolean {
  let targetStyleProp: JSExpression | null = null
  let toCheckStyleProp: JSExpression | null = null
  for (const targetAttribute of targetAttributes) {
    if (isJSXAttributesEntry(targetAttribute)) {
      if (targetAttribute.key === 'style') {
        targetStyleProp = targetAttribute.value
      } else if (
        typeof targetAttribute.key === 'string' &&
        targetAttribute.key.startsWith('data-')
      ) {
        // Ignore this case.
      } else {
        return false
      }
    }
  }
  for (const toCheckAttribute of toCheckAttributes) {
    if (isJSXAttributesEntry(toCheckAttribute)) {
      if (toCheckAttribute.key === 'style') {
        toCheckStyleProp = toCheckAttribute.value
      } else if (
        typeof toCheckAttribute.key === 'string' &&
        toCheckAttribute.key.startsWith('data-')
      ) {
        // Ignore this case.
      } else {
        return false
      }
    }
  }
  if (targetStyleProp == null && toCheckStyleProp == null) {
    return true
  } else if (targetStyleProp != null && toCheckStyleProp != null) {
    return areStylePropsCompatible(targetStyleProp, toCheckStyleProp)
  } else {
    return false
  }
}

export function isEligibleForCollapse(
  targetElement: JSXElementChild,
  elementToCheck: JSXElementChild,
): boolean {
  return (
    // Target is a JSX tag element.
    isJSXElement(targetElement) &&
    // Target only has a single child...
    targetElement.children.length === 1 &&
    // ...which is text.
    isJSXTextBlock(targetElement.children[0]) &&
    // The element we're checking is a JSX tag element.
    isJSXElement(elementToCheck) &&
    // Element to check only has a single child...
    elementToCheck.children.length === 1 &&
    // ...which is text.
    isJSXTextBlock(elementToCheck.children[0]) &&
    // The two elements share a type.
    getJSXElementNameAsString(elementToCheck.name) ===
      getJSXElementNameAsString(targetElement.name) &&
    // The two elements have compatible properties.
    arePropsCompatible(targetElement.props, elementToCheck.props)
  )
}

export function collapseTextElements(target: ElementPath, editor: EditorState): EditorState {
  const targetParent = EP.parentPath(target)
  const targetDataUID = EP.toUid(target)
  const openFile = editor.canvas.openFile?.filename

  if (openFile != null) {
    const targetElement = getElementFromProjectContents(target, editor.projectContents)
    // Identify a run of eligible elements including the target.
    const parentElement = getElementFromProjectContents(targetParent, editor.projectContents)
    if (targetElement != null && parentElement != null && isJSXElement(parentElement)) {
      let targetRun: Array<JSXElementChild> = []
      let currentRun: Array<JSXElementChild> = []

      function applyEligibleRun(): void {
        // Check the run has been added to.
        if (currentRun.length > 0) {
          // Check that the current run contains the target.
          const anythingMatchesTarget = currentRun.some((child) => {
            const childUID = getUtopiaID(child)
            return childUID === targetDataUID
          })
          if (anythingMatchesTarget) {
            targetRun = currentRun
          }

          // Clear the current run.
          currentRun = []
        }
      }

      for (const childElement of parentElement.children) {
        if (isEligibleForCollapse(targetElement, childElement)) {
          // Adding to an existing run of eligible items.
          currentRun.push(childElement)
        } else {
          // Potentially switched from an eligible case to a non-eligible one.
          applyEligibleRun()
        }
      }

      // Capture any remaining after running through the elements.
      applyEligibleRun()

      // If a run of targets was identified...
      if (targetRun.length > 0) {
        // Combine the eligible elements into a single one.
        let combinedText: string = ''
        for (const runElement of targetRun) {
          if (isJSXElement(runElement)) {
            for (const runElementChild of runElement.children) {
              if (isJSXTextBlock(runElementChild)) {
                combinedText += runElementChild.text
              }
            }
          }
        }

        // Modify the editor state.
        return modifyUnderlyingTargetElement(
          targetParent,
          editor,
          (element) => {
            if (isJSXConditionalExpression(element)) {
              return element
            }
            // Create the new collection of children by skipping those found in the target run.
            let updatedChildren: Array<JSXElementChild> = []
            for (const child of element.children) {
              if (targetRun.includes(child)) {
                if (getUtopiaID(child) === targetDataUID && isJSXElement(child)) {
                  // Insert the replacement combined element here.
                  updatedChildren.push({
                    ...child,
                    children: [jsxTextBlock(combinedText)],
                  })
                }
              } else {
                updatedChildren.push(child)
              }
            }

            // Create replacement element containing the updated children.
            return {
              ...element,
              children: updatedChildren,
            }
          },
          (parseSuccess) => parseSuccess,
        )
      }
    }
  }

  // Fallback case.
  return editor
}

// extracted into a separate function so that the intention is clear
function roundUpToPreventTextWrapping(value: number): number {
  return Math.ceil(value)
}

export function fixedSizeDimensionHandlingText(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
  dimensionValue: number,
): number {
  // Fixed dimensions for a text containing element need to be rounded up to prevent wrapping.
  const containsText = MetadataUtils.targetTextEditableAndHasText(metadata, pathTrees, elementPath)
  return containsText ? roundUpToPreventTextWrapping(dimensionValue) : dimensionValue
}

export function getLocalRectangleWithFixedWidthHandlingText(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): MaybeInfinityLocalRectangle | null {
  const localFrame = MetadataUtils.getLocalFrame(elementPath, metadata)

  if (
    localFrame == null ||
    isInfinityRectangle(localFrame) ||
    !MetadataUtils.targetTextEditableAndHasText(metadata, pathTrees, elementPath)
  ) {
    return localFrame
  }

  const nonRoundedWidth = roundUpToPreventTextWrapping(
    nullIfInfinity(
      MetadataUtils.findElementByElementPath(metadata, elementPath)?.nonRoundedGlobalFrame,
    )?.width ?? localFrame.width,
  )

  return localRectangle({
    x: localFrame.x,
    y: localFrame.y,
    height: localFrame.height,
    width: nonRoundedWidth,
  })
}
