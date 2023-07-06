import { NormalisedFrame } from 'utopia-api/core'
import {
  framePointForPinnedProp,
  LayoutPinnedProps,
  LayoutPinnedProp,
  LayoutTargetableProp,
  VerticalLayoutPinnedProps,
  HorizontalLayoutPinnedProps,
} from '../../core/layout/layout-helpers-new'
import {
  maybeSwitchLayoutProps,
  PinningAndFlexPoints,
  PinningAndFlexPointsExceptSize,
  roundJSXElementLayoutValues,
} from '../../core/layout/layout-utils'
import {
  findElementAtPath,
  findJSXElementAtPath,
  findJSXElementLikeAtPath,
  getSimpleAttributeAtPath,
  MetadataUtils,
} from '../../core/model/element-metadata-utils'
import {
  isJSXElement,
  jsExpressionValue,
  JSXElementChild,
  UtopiaJSXComponent,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  ArbitraryJSBlock,
  TopLevelElement,
  getJSXElementNameAsString,
  isJSExpressionOtherJavaScript,
  isJSXFragment,
  isUtopiaJSXComponent,
  SettableLayoutSystem,
  emptyComments,
  jsxElementName,
  jsxElementNameEquals,
  isJSXElementLike,
  isJSXConditionalExpression,
  isNullJSXAttributeValue,
} from '../../core/shared/element-template'
import {
  guaranteeUniqueUids,
  isSceneElement,
  getIndexInParent,
  insertJSXElementChild,
} from '../../core/model/element-template-utils'
import { generateUID, getUtopiaID, setUtopiaID } from '../../core/shared/uid-utils'
import {
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  ValueAtPath,
  getAllPathsFromAttributes,
} from '../../core/shared/jsx-attributes'
import {
  Imports,
  ParseSuccess,
  ElementPath,
  PropertyPath,
  isParseSuccess,
  isTextFile,
  HighlightBoundsForUids,
  ExportsDetail,
  NodeModules,
} from '../../core/shared/project-file-types'
import {
  applyUtopiaJSXComponentsChanges,
  getUtopiaJSXComponentsFromSuccess,
} from '../../core/model/project-file-utils'
import {
  eitherToMaybe,
  flatMapEither,
  foldEither,
  isRight,
  right,
  isLeft,
  Either,
} from '../../core/shared/either'
import Utils, {
  absolute,
  after,
  before,
  IndexPosition,
  shiftIndexPositionForRemovedElement,
} from '../../utils/utils'
import {
  CanvasPoint,
  canvasPoint,
  CanvasRectangle,
  canvasRectangle,
  CanvasVector,
  isInfinityRectangle,
  isFiniteRectangle,
  localRectangle,
  LocalRectangle,
  nullIfInfinity,
  Size,
} from '../../core/shared/math-utils'
import {
  DerivedState,
  EditorState,
  OriginalCanvasAndLocalFrame,
  removeElementAtPath,
  TransientCanvasState,
  transientCanvasState,
  transientFileState,
  modifyUnderlyingTargetElement,
  modifyParseSuccessAtPath,
  getOpenUIJSFileKey,
  withUnderlyingTargetFromEditorState,
  modifyUnderlyingElementForOpenFile,
  TransientFilesState,
  forUnderlyingTargetFromEditorState,
  TransientFileState,
  ResizeOptions,
  AllElementProps,
  ElementProps,
  NavigatorEntry,
  isSyntheticNavigatorEntry,
  insertElementAtPath,
} from '../editor/store/editor-state'
import * as Frame from '../frame'
import { getImageSizeFromMetadata, MultipliersForImages, scaleImageDimensions } from '../images'
import * as EP from '../../core/shared/element-path'
import * as PP from '../../core/shared/property-path'
import {
  CanvasFrameAndTarget,
  CSSCursor,
  DuplicateNewUID,
  EdgePosition,
  flexResizeChange,
  pinFrameChange,
  PinOrFlexFrameChange,
} from './canvas-types'
import {
  collectParentAndSiblingGuidelines,
  filterGuidelinesStaticAxis,
  oneGuidelinePerDimension,
} from './controls/guideline-helpers'
import {
  determineElementsToOperateOnForDragging,
  dragComponent,
  extendSelectedViewsForInteraction,
} from './controls/select-mode/move-utils'
import {
  cornerGuideline,
  Guidelines,
  GuidelineWithRelevantPoints,
  GuidelineWithSnappingVectorAndPointsOfRelevance,
  xAxisGuideline,
  yAxisGuideline,
} from './guideline'
import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { getStoryboardElementPath, getStoryboardUID } from '../../core/model/scene-utils'
import { forceNotNull, optionalMap } from '../../core/shared/optional-utils'
import { assertNever, fastForEach } from '../../core/shared/utils'
import { getContentsTreeFileFromString, ProjectContentTreeRoot } from '../assets'
import { getAllTargetsAtPointAABB } from './dom-lookup'
import { CSSNumber, parseCSSLengthPercent, printCSSNumber } from '../inspector/common/css-utils'
import { uniqBy } from '../../core/shared/array-utils'
import { mapValues } from '../../core/shared/object-utils'
import { getTopLevelName, importedFromWhere } from '../editor/import-utils'
import { Notice } from '../common/notice'
import { createStylePostActionToast } from '../../core/layout/layout-notice'
import { includeToast, uniqToasts } from '../editor/actions/toast-helpers'
import { stylePropPathMappingFn } from '../inspector/common/property-path-hooks'
import { EditorDispatch } from '../editor/action-types'
import { styleStringInArray } from '../../utils/common-constants'
import { treatElementAsFragmentLike } from './canvas-strategies/strategies/fragment-like-helpers'
import { mergeImports } from '../../core/workers/common/project-file-utils'
import {
  childInsertionPath,
  conditionalClauseInsertionPath,
  getInsertionPathWithSlotBehavior,
} from '../editor/store/insertion-path'
import { getConditionalCaseCorrespondingToBranchPath } from '../../core/model/conditionals'
import { isEmptyConditionalBranch } from '../../core/model/conditionals'
import { ElementPathTrees } from '../../core/shared/element-path-tree'
import { getAllUniqueUids } from '../../core/model/get-unique-ids'
import { ErrorMessage } from '../../core/shared/error-messages'
import { OverlayError } from '../../core/shared/runtime-report-logs'

function dragDeltaScaleForProp(prop: LayoutTargetableProp): number {
  switch (prop) {
    case 'right':
    case 'bottom':
      return -1
    case 'flexGrow':
    case 'flexShrink':
      return 0.01
    default:
      return 1
  }
}

function unsetValueWhenNegative(prop: LayoutTargetableProp): boolean {
  switch (prop) {
    case 'left':
    case 'top':
    case 'right':
    case 'bottom':
      return false
    default:
      return true
  }
}

function cssNumberAsNumberIfPossible(
  cssNumber: CSSNumber | number | string | undefined,
): number | string | undefined {
  if (cssNumber == null) {
    return undefined
  } else {
    switch (typeof cssNumber) {
      case 'string':
        return cssNumber
      case 'number':
        return cssNumber
      default:
        if (cssNumber.unit == null) {
          return cssNumber.value
        } else {
          return printCSSNumber(cssNumber, null)
        }
    }
  }
}

function referenceParentValueForProp(prop: LayoutPinnedProp, parentSize: Size): number {
  switch (prop) {
    case 'left':
    case 'top':
    case 'width':
    case 'height':
      return 0
    case 'right':
      return parentSize.width
    case 'bottom':
      return parentSize.height
    default:
      const _exhaustiveCheck: never = prop
      throw new Error(`Unknown frame point ${JSON.stringify(prop)}`)
  }
}

function valueToUseForPin(
  prop: LayoutPinnedProp,
  absoluteValue: number,
  pinIsPercentPin: boolean,
  parentRect: Size,
): string | number {
  const referenceSize = HorizontalLayoutPinnedProps.includes(prop)
    ? parentRect.width
    : parentRect.height
  const referenceValue = referenceParentValueForProp(prop, parentRect)
  const shouldInvertOffset = prop === 'right' || prop === 'bottom'
  const actualOffsetValue = absoluteValue - referenceValue
  const offsetValue = shouldInvertOffset ? -actualOffsetValue : actualOffsetValue
  if (pinIsPercentPin) {
    const percentValue = (offsetValue / referenceSize) * 100
    return `${percentValue}%`
  } else {
    return offsetValue
  }
}

export function updateFramesOfScenesAndComponents(
  editorState: EditorState,
  framesAndTargets: Array<PinOrFlexFrameChange>,
  optionalParentFrame: CanvasRectangle | null,
): EditorState {
  let workingEditorState: EditorState = editorState
  let toastsToAdd: Array<Notice> = []
  Utils.fastForEach(framesAndTargets, (frameAndTarget) => {
    const target = frameAndTarget.target
    // Realign to aim at the static version, not the dynamic one.
    const originalTarget = target
    const staticTarget = EP.dynamicPathToStaticPath(target)
    if (staticTarget == null) {
      return
    }

    const element = withUnderlyingTargetFromEditorState(
      staticTarget,
      workingEditorState,
      null,
      (success, underlyingElement) => underlyingElement,
    )
    if (element == null || !(isJSXElement(element) || isJSXConditionalExpression(element))) {
      throw new Error(`Unexpected result when looking for element: ${element}`)
    }

    const staticParentPath = EP.parentPath(staticTarget)
    const parentElement = withUnderlyingTargetFromEditorState(
      staticParentPath,
      workingEditorState,
      null,
      (success, underlyingElement) => underlyingElement,
    )

    const elementMetadata = MetadataUtils.findElementByElementPath(editorState.jsxMetadata, target)
    const elementProps = editorState.allElementProps[EP.toString(target)] ?? {}

    const elementAttributes = isJSXElement(element) ? element.props : []

    const isFlexContainer =
      frameAndTarget.type !== 'PIN_FRAME_CHANGE' &&
      frameAndTarget.type !== 'PIN_MOVE_CHANGE' &&
      frameAndTarget.type !== 'PIN_SIZE_CHANGE' &&
      frameAndTarget.type !== 'SINGLE_RESIZE' // TODO since now we are trusting the frameAndTarget.type, there is no point in having two switches

    let propsToSet: Array<ValueAtPath> = []
    let propsToSkip: Array<PropertyPath> = []
    let propsToUnset: Array<PropertyPath> = []
    if (isFlexContainer) {
      switch (frameAndTarget.type) {
        case 'FLEX_MOVE':
          workingEditorState = modifyUnderlyingElementForOpenFile(
            originalTarget,
            workingEditorState,
            (elem) => elem,
            (success, underlyingTarget) => {
              const components = getUtopiaJSXComponentsFromSuccess(success)
              if (underlyingTarget == null) {
                return success
              } else {
                const updatedComponents = reorderComponent(
                  workingEditorState.projectContents,
                  components,
                  underlyingTarget,
                  absolute(frameAndTarget.newIndex),
                )
                return {
                  ...success,
                  topLevelElements: applyUtopiaJSXComponentsChanges(
                    success.topLevelElements,
                    updatedComponents,
                  ),
                }
              }
            },
          )
          break
        case 'FLEX_RESIZE':
          if (staticParentPath == null) {
            throw new Error(`No parent available for ${JSON.stringify(staticParentPath)}`)
          } else {
            if (parentElement == null) {
              throw new Error(`Unexpected result when looking for parent: ${parentElement}`)
            }

            const targetPropertyPath = stylePropPathMappingFn(frameAndTarget.targetProperty, [
              'style',
            ])
            const valueFromDOM = getObservableValueForLayoutProp(
              elementMetadata,
              frameAndTarget.targetProperty,
              elementProps,
            )
            const valueFromAttributes = eitherToMaybe(
              getSimpleAttributeAtPath(right(elementAttributes), targetPropertyPath),
            )
            // Defer through these in order: observable value >>> value from attribute >>> 0.
            const currentAttributeToChange = valueFromDOM ?? valueFromAttributes ?? 0
            const scalingFactor = dragDeltaScaleForProp(frameAndTarget.targetProperty)
            const scaledDelta = Math.floor(frameAndTarget.delta * scalingFactor)
            const newAttributeNumericValue = currentAttributeToChange + scaledDelta
            const shouldUnsetDraggedProp =
              newAttributeNumericValue < 0 && unsetValueWhenNegative(frameAndTarget.targetProperty)

            if (shouldUnsetDraggedProp) {
              propsToUnset.push(targetPropertyPath)
            } else {
              const newAttributeValue = jsExpressionValue(newAttributeNumericValue, emptyComments)

              propsToSet.push({
                path: targetPropertyPath,
                value: newAttributeValue,
              })
            }

            propsToSkip.push(
              stylePropPathMappingFn('left', styleStringInArray),
              stylePropPathMappingFn('top', styleStringInArray),
              stylePropPathMappingFn('right', styleStringInArray),
              stylePropPathMappingFn('bottom', styleStringInArray),
              stylePropPathMappingFn('width', styleStringInArray),
              stylePropPathMappingFn('height', styleStringInArray),
              stylePropPathMappingFn('minWidth', styleStringInArray),
              stylePropPathMappingFn('minHeight', styleStringInArray),
              stylePropPathMappingFn('maxWidth', styleStringInArray),
              stylePropPathMappingFn('maxHeight', styleStringInArray),
              stylePropPathMappingFn('flexBasis', styleStringInArray),
              stylePropPathMappingFn('flexGrow', styleStringInArray),
              stylePropPathMappingFn('flexShrink', styleStringInArray),
            )
          }
          break
        default:
          const _exhaustiveCheck: never = frameAndTarget
          throw new Error(`Unhandled type ${JSON.stringify(frameAndTarget)}`)
      }
    } else {
      let parentFrame: CanvasRectangle | null = null
      if (optionalParentFrame == null) {
        const nonGroupParent = MetadataUtils.findParent(
          workingEditorState.jsxMetadata,
          originalTarget,
        )
        parentFrame =
          nonGroupParent == null
            ? null
            : nullIfInfinity(
                MetadataUtils.getFrameInCanvasCoords(
                  nonGroupParent,
                  workingEditorState.jsxMetadata,
                ),
              )
      } else {
        parentFrame = optionalParentFrame
      }
      const parentOffset =
        parentFrame == null
          ? ({ x: 0, y: 0 } as CanvasPoint)
          : ({ x: parentFrame.x, y: parentFrame.y } as CanvasPoint)

      switch (frameAndTarget.type) {
        case 'PIN_FRAME_CHANGE':
        case 'PIN_SIZE_CHANGE':
          if (frameAndTarget.frame != null) {
            const newLocalFrame = Utils.getLocalRectangleInNewParentContext(
              parentOffset,
              frameAndTarget.frame,
            )
            const currentLocalFrame = nullIfInfinity(
              MetadataUtils.getFrame(target, workingEditorState.jsxMetadata),
            )
            const currentFullFrame = optionalMap(Frame.getFullFrame, currentLocalFrame)
            const fullFrame = Frame.getFullFrame(newLocalFrame)

            // Pinning layout.
            const frameProps = LayoutPinnedProps.filter((p) => {
              const value = getLayoutProperty(p, right(elementAttributes), styleStringInArray)
              return isLeft(value) || value.value != null
            })

            function whichPropsToUpdate(): Array<LayoutPinnedProp> {
              if (frameAndTarget.type === 'PIN_SIZE_CHANGE') {
                // only update left, top, right or bottom if the frame is expressed as left, top, right, bottom.
                // otherwise try to change width and height only
                let verticalPoints = frameProps.filter((p) => VerticalLayoutPinnedProps.includes(p))
                let horizontalPoints = frameProps.filter((p) =>
                  HorizontalLayoutPinnedProps.includes(p),
                )
                if (verticalPoints.length < 2) {
                  verticalPoints.push('height')
                }
                if (horizontalPoints.length < 2) {
                  horizontalPoints.push('width')
                }

                return [...horizontalPoints, ...verticalPoints]
              } else if (
                frameAndTarget.type === 'PIN_FRAME_CHANGE' &&
                frameAndTarget.edgePosition != null &&
                isEdgePositionOnSide(frameAndTarget.edgePosition)
              ) {
                // if it has partial positioning points set and dragged on an edge only the dragged edge should be added while keeping the existing frame points.
                return extendPartialFramePointsForResize(frameProps, frameAndTarget.edgePosition)
              } else {
                // The "Old" behavior, for PIN_FRAME_CHANGE
                return frameProps.length == 4 ? frameProps : ['left', 'top', 'width', 'height']
              }
            }

            const propsToUpdate = whichPropsToUpdate()

            Utils.fastForEach(propsToUpdate, (propToUpdate) => {
              const absoluteValue = fullFrame[propToUpdate]
              const previousValue = currentFullFrame == null ? null : currentFullFrame[propToUpdate]

              const propPathToUpdate = stylePropPathMappingFn(propToUpdate, styleStringInArray)
              const existingProp = getLayoutProperty(propToUpdate, right(elementAttributes), [
                'style',
              ])
              if (absoluteValue === previousValue || isLeft(existingProp)) {
                // Only update pins that have actually changed or aren't set via code
                propsToSkip.push(propPathToUpdate)
              } else {
                const pinIsPercentage =
                  existingProp.value == null ? false : existingProp.value.unit === '%'
                let valueToUse: string | number
                if (parentFrame == null) {
                  valueToUse = absoluteValue
                } else {
                  valueToUse = valueToUseForPin(
                    propToUpdate,
                    absoluteValue,
                    pinIsPercentage,
                    parentFrame,
                  )
                }
                propsToSet.push({
                  path: propPathToUpdate,
                  value: jsExpressionValue(valueToUse, emptyComments),
                })
              }
            })
          }
          break

        case 'PIN_MOVE_CHANGE': {
          let frameProps: { [k: string]: string | number | undefined } = {}
          Utils.fastForEach(LayoutPinnedProps, (p) => {
            if (p !== 'width' && p !== 'height') {
              const value = getLayoutProperty(p, right(elementAttributes), styleStringInArray)
              if (isLeft(value) || value.value != null) {
                frameProps[p] = cssNumberAsNumberIfPossible(value.value)
                propsToSkip.push(stylePropPathMappingFn(p, styleStringInArray))
              }
            }
          })

          let framePointsToUse: Array<LayoutPinnedProp> = [
            ...(Object.keys(frameProps) as Array<LayoutPinnedProp>),
          ]
          const horizontalExistingFramePoints = framePointsToUse.filter(
            (p) => p === 'left' || p === 'right',
          )
          if (horizontalExistingFramePoints.length === 0) {
            framePointsToUse.push('left')
          }
          const verticalExistingFramePoints = framePointsToUse.filter(
            (p) => p === 'top' || p === 'bottom',
          )
          if (verticalExistingFramePoints.length === 0) {
            framePointsToUse.push('top')
          }
          propsToSet.push(
            ...getPropsToSetToMoveElement(
              frameAndTarget.delta,
              framePointsToUse,
              frameProps,
              parentFrame,
            ),
          )
          break
        }
        case 'SINGLE_RESIZE':
          let frameProps: { [k: string]: string | number | undefined } = {}
          Utils.fastForEach(LayoutPinnedProps, (p) => {
            const framePoint = framePointForPinnedProp(p)
            const value = getLayoutProperty(p, right(elementAttributes), styleStringInArray)
            if (isLeft(value) || value.value != null) {
              frameProps[framePoint] = cssNumberAsNumberIfPossible(value.value)
              propsToSkip.push(stylePropPathMappingFn(p, styleStringInArray))
            }
          })

          let framePointsToUse: Array<LayoutPinnedProp> = Object.keys(
            frameProps,
          ) as Array<LayoutPinnedProp>

          if (isEdgePositionOnSide(frameAndTarget.edgePosition)) {
            framePointsToUse = extendPartialFramePointsForResize(
              framePointsToUse,
              frameAndTarget.edgePosition,
            )
          } else {
            let verticalPoints = framePointsToUse.filter((p) => {
              return VerticalLayoutPinnedProps.includes(p)
            })
            let horizontalPoints = framePointsToUse.filter((p) => {
              return HorizontalLayoutPinnedProps.includes(p)
            })

            if (verticalPoints.length < 2) {
              if (verticalPoints.length === 0) {
                verticalPoints.push('top')
              }
              verticalPoints.push('height')
            }
            if (horizontalPoints.length < 2) {
              if (horizontalPoints.length === 0) {
                horizontalPoints.push('left')
              }
              horizontalPoints.push('width')
            }

            framePointsToUse = Utils.uniq([...verticalPoints, ...horizontalPoints])
          }

          propsToSet.push(
            ...getPropsToSetToResizeElement(
              frameAndTarget.edgePosition,
              frameAndTarget.sizeDelta.x,
              frameAndTarget.sizeDelta.y,
              framePointsToUse,
              frameProps,
              parentFrame,
            ),
          )
          break
        default:
          const _exhaustiveCheck: never = frameAndTarget
          throw new Error(`Unhandled type ${JSON.stringify(frameAndTarget)}`)
      }
    }

    if (propsToSet.length > 0 || propsToUnset.length > 0) {
      const propsToNotDelete = [...propsToSet.map((p) => p.path), ...propsToSkip]

      workingEditorState = modifyUnderlyingElementForOpenFile(
        originalTarget,
        workingEditorState,
        (elem) => {
          // Remove the pinning and flex props first...
          const propsToMaybeRemove: Array<LayoutPinnedProp | 'flexBasis'> =
            frameAndTarget.type === 'PIN_MOVE_CHANGE'
              ? PinningAndFlexPointsExceptSize // for PIN_MOVE_CHANGE, we don't want to remove the size props, we just keep them intact
              : PinningAndFlexPoints
          let propsToRemove: Array<PropertyPath> = [...propsToUnset]
          fastForEach(propsToMaybeRemove, (prop) => {
            const propPath = stylePropPathMappingFn(prop, styleStringInArray)
            if (!PP.contains(propsToNotDelete, propPath)) {
              propsToRemove.push(propPath)
            }
          })
          const layoutPropsRemoved = unsetJSXValuesAtPaths(elem.props, propsToRemove)
          // ...Add in the updated properties.

          const layoutPropsAdded = flatMapEither(
            (props) => setJSXValuesAtPaths(props, propsToSet),
            layoutPropsRemoved,
          )

          return foldEither(
            (_) => elem,
            (updatedProps) => {
              toastsToAdd.push(
                ...createStylePostActionToast(
                  MetadataUtils.getElementLabel(
                    workingEditorState.allElementProps,
                    originalTarget,
                    workingEditorState.elementPathTree,
                    workingEditorState.jsxMetadata,
                  ),
                  getAllPathsFromAttributes(elem.props),
                  getAllPathsFromAttributes(updatedProps),
                ),
              )
              return {
                ...elem,
                props: updatedProps,
              }
            },
            layoutPropsAdded,
          )
        },
      )
    }

    // Round the frame details.
    workingEditorState = modifyUnderlyingElementForOpenFile(
      staticTarget,
      workingEditorState,
      (attrs) => roundJSXElementLayoutValues(styleStringInArray, attrs),
    )
    // TODO originalFrames is never being set, so we have a regression here, meaning keepChildrenGlobalCoords
    // doesn't work. Once that is fixed we can re-implement keeping the children in place
  })

  if (toastsToAdd.length > 0) {
    workingEditorState = {
      ...workingEditorState,
      toasts: uniqToasts([...workingEditorState.toasts, ...toastsToAdd]),
    }
  }
  return workingEditorState
}

function updateFrameValueForProp(
  framePoint: LayoutPinnedProp,
  delta: number,
  frameProps: { [k: string]: string | number | undefined },
  parentFrame: CanvasRectangle | null,
): ValueAtPath | null {
  if (delta !== 0) {
    const existingProp = frameProps[framePoint]
    if (existingProp == null) {
      return {
        path: stylePropPathMappingFn(framePoint, styleStringInArray),
        value: jsExpressionValue(delta, emptyComments),
      }
    }
    const parsedProp = foldEither(
      (_) => null,
      (r) => r,
      parseCSSLengthPercent(existingProp),
    )
    if (parsedProp != null) {
      const pinIsPercentage = parsedProp.unit === '%'
      const pinIsUnitlessOrPx = parsedProp.unit == null || parsedProp.unit === 'px'
      if (pinIsPercentage) {
        let valueToUse: string | number
        const percentValue = parsedProp.value
        if (parentFrame != null) {
          const referenceSize = HorizontalLayoutPinnedProps.includes(framePoint)
            ? parentFrame.width
            : parentFrame.height
          const deltaAsPercentValue = (delta / referenceSize) * 100
          valueToUse = `${percentValue + deltaAsPercentValue}%`
        } else {
          valueToUse = `${percentValue + delta}%`
        }
        return {
          path: stylePropPathMappingFn(framePoint, styleStringInArray),
          value: jsExpressionValue(valueToUse, emptyComments),
        }
      } else if (pinIsUnitlessOrPx) {
        return {
          path: stylePropPathMappingFn(framePoint, styleStringInArray),
          value: jsExpressionValue(parsedProp.value + delta, emptyComments),
        }
      }
    }
  }
  return null
}

export function getPropsToSetToMoveElement(
  dragDelta: CanvasVector,
  framePoints: Array<LayoutPinnedProp>,
  frameProps: { [k: string]: string | number | undefined },
  parentFrame: CanvasRectangle | null,
): ValueAtPath[] {
  let propsToSet: ValueAtPath[] = []
  Utils.fastForEach(framePoints, (framePoint) => {
    const delta = HorizontalLayoutPinnedProps.includes(framePoint) ? dragDelta.x : dragDelta.y
    const shouldInvertValue = framePoint === 'right' || framePoint === 'bottom'
    const updatedProp = updateFrameValueForProp(
      framePoint,
      shouldInvertValue ? -delta : delta,
      frameProps,
      parentFrame,
    )
    if (updatedProp != null) {
      propsToSet.push(updatedProp)
    }
  })
  return propsToSet
}

function getPropsToSetToResizeElement(
  edgePosition: EdgePosition,
  widthDelta: number,
  heightDelta: number,
  framePoints: Array<LayoutPinnedProp>,
  frameProps: { [k: string]: string | number | undefined },
  parentFrame: CanvasRectangle | null,
): ValueAtPath[] {
  let propsToSet: ValueAtPath[] = []
  Utils.fastForEach(framePoints, (framePoint) => {
    let updatedProp
    switch (framePoint) {
      case 'left': {
        const targetEdgePoint = { x: 0, y: 0.5 }
        const delta = widthDelta * (edgePosition.x + targetEdgePoint.x - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case 'top': {
        const targetEdgePoint = { x: 0.5, y: 0 }
        const delta = heightDelta * (edgePosition.y + targetEdgePoint.y - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case 'right': {
        const targetEdgePoint = { x: 1, y: 0.5 }
        const delta = widthDelta * -(edgePosition.x + targetEdgePoint.x - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case 'bottom': {
        const targetEdgePoint = { x: 0.5, y: 1 }
        const delta = heightDelta * -(edgePosition.y + targetEdgePoint.y - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case 'width': {
        if (widthDelta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, widthDelta, frameProps, parentFrame)
        }
        break
      }
      case 'height': {
        if (heightDelta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, heightDelta, frameProps, parentFrame)
        }
        break
      }
      default: {
        break
      }
    }
    if (updatedProp != null) {
      propsToSet.push(updatedProp)
    }
  })
  return propsToSet
}

function extendPartialFramePointsForResize(
  frameProps: Array<LayoutPinnedProp>,
  edgePosition: EdgePosition,
): Array<LayoutPinnedProp> {
  // if it has partial positioning points set and dragged on an edge only the dragged edge should be added while keeping the existing frame points.
  let verticalPoints = frameProps.filter((p) => VerticalLayoutPinnedProps.includes(p))
  let horizontalPoints = frameProps.filter((p) => HorizontalLayoutPinnedProps.includes(p))
  let framePointsToUse = [...frameProps]
  if (edgePosition.x === 0.5 && verticalPoints.length < 2) {
    if (verticalPoints.length === 0) {
      if (edgePosition.y === 0) {
        verticalPoints.push('top')
        verticalPoints.push('height')
      } else {
        verticalPoints.push('height')
      }
    } else {
      if (edgePosition.y === 0) {
        verticalPoints.push('top')
      } else if (!verticalPoints.includes('bottom')) {
        verticalPoints.push('height')
      }
    }
    framePointsToUse = [...verticalPoints, ...horizontalPoints]
  }
  if (edgePosition.y === 0.5 && horizontalPoints.length < 2) {
    if (horizontalPoints.length === 0) {
      if (edgePosition.x === 0) {
        horizontalPoints.push('left')
        horizontalPoints.push('width')
      } else {
        horizontalPoints.push('width')
      }
    } else {
      if (edgePosition.x === 0) {
        horizontalPoints.push('left')
      } else if (!horizontalPoints.includes('right')) {
        horizontalPoints.push('width')
      }
    }
    framePointsToUse = [...verticalPoints, ...horizontalPoints]
  }
  return Utils.uniq(framePointsToUse)
}

export function pickPointOnRect(rect: CanvasRectangle, position: EdgePosition): CanvasPoint {
  return Utils.addVectors(
    Utils.rectOrigin(rect),
    Utils.scalePoint(Utils.rectSizeToVector(rect), position as CanvasPoint),
  )
}

export function isEdgePositionACorner(edgePosition: EdgePosition): boolean {
  return (
    (edgePosition.x === 0 && edgePosition.y === 0) ||
    (edgePosition.x === 0 && edgePosition.y === 1) ||
    (edgePosition.x === 1 && edgePosition.y === 0) ||
    (edgePosition.x === 1 && edgePosition.y === 1)
  )
}

export function isEdgePositionAHorizontalEdge(edgePosition: EdgePosition): boolean {
  return (
    (edgePosition.x === 0.5 && edgePosition.y === 0) ||
    (edgePosition.x === 0.5 && edgePosition.y === 1)
  )
}

export function isEdgePositionAVerticalEdge(edgePosition: EdgePosition): boolean {
  return (
    (edgePosition.x === 0 && edgePosition.y === 0.5) ||
    (edgePosition.x === 1 && edgePosition.y === 0.5)
  )
}

export function isEdgePositionOnSide(edgePosition: EdgePosition): boolean {
  return edgePosition.x === 0.5 || edgePosition.y === 0.5
}

export const SnappingThreshold = 5

export function collectGuidelines(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  scale: number,
  draggedPoint: CanvasPoint | null,
  resizingFromPosition: EdgePosition | null,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
): Array<GuidelineWithSnappingVectorAndPointsOfRelevance> {
  if (draggedPoint == null) {
    return []
  }

  let guidelines: Array<GuidelineWithRelevantPoints> = collectParentAndSiblingGuidelines(
    metadata,
    allElementProps,
    pathTrees,
    selectedViews,
  )

  // For any images create guidelines at the current multiplier setting.
  if (resizingFromPosition != null) {
    Utils.fastForEach(selectedViews, (selectedView) => {
      if (MetadataUtils.isPinnedAndNotAbsolutePositioned(metadata, selectedView)) {
        return
      }

      const instance = MetadataUtils.findElementByElementPath(metadata, selectedView)
      if (
        instance != null &&
        MetadataUtils.isImg(instance) &&
        instance.localFrame != null &&
        isFiniteRectangle(instance.localFrame)
      ) {
        const frame = instance.localFrame
        const imageSize = getImageSizeFromMetadata(allElementProps, instance)
        Utils.fastForEach(MultipliersForImages, (multiplier) => {
          const imageDimension = scaleImageDimensions(imageSize, multiplier)
          // Calculate the guidelines around the corner/edge given.
          const point: CanvasPoint = {
            x: frame.x + frame.width * resizingFromPosition.x,
            y: frame.y + frame.width * resizingFromPosition.y,
          } as CanvasPoint
          const lowHalfWidth = Utils.roundTo(imageDimension.width / 2, 0)
          const highHalfWidth = imageDimension.width - lowHalfWidth
          const lowHalfHeight = Utils.roundTo(imageDimension.height / 2, 0)
          const highHalfHeight = imageDimension.height - lowHalfHeight
          if (isEdgePositionACorner(resizingFromPosition)) {
            // If this is a corner the guidelines will be at x +/- width and y +/- height.
            if (resizingFromPosition.x === 0) {
              if (resizingFromPosition.y === 0) {
                // Top-left.
                guidelines.push({
                  guideline: cornerGuideline(
                    point.x + imageDimension.width,
                    point.y + imageDimension.height,
                    -imageDimension.width,
                    -imageDimension.height,
                  ),
                  pointsOfRelevance: [
                    canvasPoint({
                      x: point.x + imageDimension.width,
                      y: point.y + imageDimension.height,
                    }),
                  ],
                })
              } else {
                // Bottom-left.
                guidelines.push({
                  guideline: cornerGuideline(
                    point.x,
                    point.y + imageDimension.height,
                    imageDimension.width,
                    -imageDimension.height,
                  ),
                  pointsOfRelevance: [
                    canvasPoint({
                      x: point.x,
                      y: point.y + imageDimension.height,
                    }),
                  ],
                })
              }
            } else {
              if (resizingFromPosition.y === 0) {
                // Top-right.
                guidelines.push({
                  guideline: cornerGuideline(
                    point.x + imageDimension.width,
                    point.y,
                    -imageDimension.width,
                    imageDimension.height,
                  ),
                  pointsOfRelevance: [
                    canvasPoint({
                      x: point.x + imageDimension.width,
                      y: point.y,
                    }),
                  ],
                })
              } else {
                // Bottom-right.
                guidelines.push({
                  guideline: cornerGuideline(
                    point.x,
                    point.y,
                    imageDimension.width,
                    imageDimension.height,
                  ),
                  pointsOfRelevance: [
                    canvasPoint({
                      x: point.x,
                      y: point.y,
                    }),
                  ],
                })
              }
            }
          } else if (isEdgePositionAVerticalEdge(resizingFromPosition)) {
            // If this is a side edge the guidelines will be at x +/- width and y +/- (height / 2).
            guidelines.push(
              {
                guideline: xAxisGuideline(
                  point.x - imageDimension.width,
                  point.y - lowHalfHeight,
                  point.y + highHalfHeight,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x - imageDimension.width, y: point.y - lowHalfHeight }),
                  canvasPoint({ x: point.x - imageDimension.width, y: point.y + highHalfHeight }),
                ],
              },
              {
                guideline: xAxisGuideline(
                  point.x + imageDimension.width,
                  point.y - lowHalfHeight,
                  point.y + highHalfHeight,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x + imageDimension.width, y: point.y - lowHalfHeight }),
                  canvasPoint({ x: point.x + imageDimension.width, y: point.y + highHalfHeight }),
                ],
              },
              {
                guideline: yAxisGuideline(
                  point.y - lowHalfHeight,
                  point.x - imageDimension.width,
                  point.x,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x - imageDimension.width, y: point.y - lowHalfHeight }),
                  canvasPoint({ x: point.x, y: point.y - lowHalfHeight }),
                ],
              },
              {
                guideline: yAxisGuideline(
                  point.y - lowHalfHeight,
                  point.x,
                  point.x + imageDimension.width,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x + imageDimension.width, y: point.y - lowHalfHeight }),
                  canvasPoint({ x: point.x, y: point.y - lowHalfHeight }),
                ],
              },
              {
                guideline: yAxisGuideline(
                  point.y + highHalfHeight,
                  point.x - imageDimension.width,
                  point.x,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x - imageDimension.width, y: point.y + lowHalfHeight }),
                  canvasPoint({ x: point.x, y: point.y + lowHalfHeight }),
                ],
              },
              {
                guideline: yAxisGuideline(
                  point.y + highHalfHeight,
                  point.x,
                  point.x + imageDimension.width,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x + imageDimension.width, y: point.y + lowHalfHeight }),
                  canvasPoint({ x: point.x, y: point.y + lowHalfHeight }),
                ],
              },
            )
          } else if (isEdgePositionAHorizontalEdge(resizingFromPosition)) {
            // If this is a top/bottom edge the guidelines will be at x +/- (width / 2) and y +/- height.
            guidelines.push(
              {
                guideline: xAxisGuideline(
                  point.x - lowHalfWidth,
                  point.y - imageDimension.height,
                  point.y,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x - lowHalfWidth, y: point.y - imageDimension.height }),
                  canvasPoint({ x: point.x - lowHalfWidth, y: point.y }),
                ],
              },
              {
                guideline: xAxisGuideline(
                  point.x - lowHalfWidth,
                  point.y,
                  point.y + imageDimension.height,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x - lowHalfWidth, y: point.y + imageDimension.height }),
                  canvasPoint({ x: point.x - lowHalfWidth, y: point.y }),
                ],
              },
              {
                guideline: xAxisGuideline(
                  point.x + highHalfWidth,
                  point.y - imageDimension.height,
                  point.y,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x + highHalfWidth, y: point.y - imageDimension.height }),
                  canvasPoint({ x: point.x + highHalfHeight, y: point.y }),
                ],
              },
              {
                guideline: xAxisGuideline(
                  point.x + highHalfWidth,
                  point.y,
                  point.y + imageDimension.height,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x + highHalfWidth, y: point.y + imageDimension.height }),
                  canvasPoint({ x: point.x + highHalfHeight, y: point.y }),
                ],
              },
              {
                guideline: yAxisGuideline(
                  point.y - imageDimension.height,
                  point.x - lowHalfWidth,
                  point.x + highHalfWidth,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x - lowHalfWidth, y: point.y - imageDimension.height }),
                  canvasPoint({ x: point.x + highHalfHeight, y: point.y - imageDimension.height }),
                ],
              },
              {
                guideline: yAxisGuideline(
                  point.y + imageDimension.height,
                  point.x - lowHalfWidth,
                  point.x + highHalfWidth,
                ),
                pointsOfRelevance: [
                  canvasPoint({ x: point.x - lowHalfWidth, y: point.y + imageDimension.height }),
                  canvasPoint({ x: point.x + highHalfHeight, y: point.y + imageDimension.height }),
                ],
              },
            )
          }
        })
      }
    })
  }
  const filteredGuidelines =
    resizingFromPosition != null
      ? filterGuidelinesStaticAxis((g) => g.guideline, guidelines, resizingFromPosition)
      : guidelines
  const closestGuidelines = Guidelines.getClosestGuidelinesAndOffsets(
    [draggedPoint.x],
    [draggedPoint.y],
    [draggedPoint],
    filteredGuidelines,
    null,
    SnappingThreshold,
    scale,
  )
  return closestGuidelines
}

function innerSnapPoint(
  selectedViews: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
  point: CanvasPoint,
  resizingFromPosition: EdgePosition | null,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
): {
  point: CanvasPoint
  snappedGuideline: GuidelineWithSnappingVectorAndPointsOfRelevance | null
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
} {
  const guidelines = oneGuidelinePerDimension(
    collectGuidelines(
      jsxMetadata,
      selectedViews,
      canvasScale,
      point,
      resizingFromPosition,
      allElementProps,
      pathTrees,
    ),
  )
  let snappedPoint = point
  let snappedGuideline: GuidelineWithSnappingVectorAndPointsOfRelevance | null = null

  guidelines.forEach((guideline) => {
    snappedPoint = Utils.offsetPoint(snappedPoint, guideline.snappingVector)
    snappedGuideline = guideline
  })
  return {
    point: snappedPoint,
    snappedGuideline: snappedGuideline,
    guidelinesWithSnappingVector: guidelines,
  }
}

export function snapPoint(
  selectedViews: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  canvasScale: number,
  pointToSnap: CanvasPoint,
  enableSnapping: boolean,
  keepAspectRatio: boolean,
  diagonalA: CanvasPoint,
  diagonalB: CanvasPoint,
  resizingFromPosition: EdgePosition | null,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
): {
  snappedPointOnCanvas: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVectorAndPointsOfRelevance>
} {
  const elementsToTarget = determineElementsToOperateOnForDragging(
    selectedViews,
    jsxMetadata,
    true,
    false,
  )
  const anythingPinnedAndNotAbsolutePositioned = elementsToTarget.some((elementToTarget) => {
    return MetadataUtils.isPinnedAndNotAbsolutePositioned(jsxMetadata, elementToTarget)
  })
  const anyElementFragmentLike = selectedViews.some((elementPath) =>
    treatElementAsFragmentLike(jsxMetadata, allElementProps, pathTrees, elementPath),
  )
  const shouldSnap =
    enableSnapping && (anyElementFragmentLike || !anythingPinnedAndNotAbsolutePositioned)

  if (keepAspectRatio) {
    const closestPointOnLine = Utils.closestPointOnLine(diagonalA, diagonalB, pointToSnap)
    if (shouldSnap) {
      const { snappedGuideline: guideline, guidelinesWithSnappingVector } = innerSnapPoint(
        selectedViews,
        jsxMetadata,
        canvasScale,
        closestPointOnLine,
        resizingFromPosition,
        allElementProps,
        pathTrees,
      )
      if (guideline != null) {
        const guidelinePoints = Guidelines.convertGuidelineToPoints(guideline.guideline)
        // for now, because scale is not a first-class citizen, we know that CanvasVector and LocalVector have the same dimensions
        let snappedPoint: CanvasPoint | null = null
        switch (guidelinePoints.type) {
          case 'cornerguidelinepoint':
            snappedPoint = guidelinePoints.point
            break
          default:
            snappedPoint = Utils.lineIntersection(
              diagonalA,
              diagonalB,
              guidelinePoints.start,
              guidelinePoints.end,
            )
        }
        if (snappedPoint != null) {
          return {
            snappedPointOnCanvas: snappedPoint,
            guidelinesWithSnappingVector: guidelinesWithSnappingVector,
          }
        }
      }
      // fallback to regular diagonal snapping
      return { snappedPointOnCanvas: closestPointOnLine, guidelinesWithSnappingVector: [] }
    } else {
      return { snappedPointOnCanvas: pointToSnap, guidelinesWithSnappingVector: [] }
    }
  } else {
    const { point, guidelinesWithSnappingVector } = innerSnapPoint(
      selectedViews,
      jsxMetadata,
      canvasScale,
      pointToSnap,
      resizingFromPosition,
      allElementProps,
      pathTrees,
    )
    return shouldSnap
      ? {
          snappedPointOnCanvas: point,
          guidelinesWithSnappingVector: guidelinesWithSnappingVector,
        }
      : { snappedPointOnCanvas: pointToSnap, guidelinesWithSnappingVector: [] }
  }
}

export function isTargetPropertyHorizontal(edgePosition: EdgePosition): boolean {
  return edgePosition.x !== 0.5
}

export const SkipFrameChange = 'skipFrameChange'

export interface MoveTemplateResult {
  updatedEditorState: EditorState
  newPath: ElementPath | null
}

export function getFrameChange(
  target: ElementPath,
  newFrame: CanvasRectangle,
  isParentFlex: boolean,
): PinOrFlexFrameChange {
  if (isParentFlex) {
    return flexResizeChange(target, 'flexBasis', 0) // KILLME
  } else {
    return pinFrameChange(target, newFrame, null)
  }
}

export function moveTemplate(
  target: ElementPath,
  originalPath: ElementPath,
  newFrame: CanvasRectangle | typeof SkipFrameChange | null,
  indexPosition: IndexPosition,
  newParentPath: ElementPath | null,
  parentFrame: CanvasRectangle | null,
  editorState: EditorState,
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  highlightedViews: Array<ElementPath>,
  newParentLayoutSystem: SettableLayoutSystem | null,
  newParentMainAxis: 'horizontal' | 'vertical' | null,
): MoveTemplateResult {
  function noChanges(): MoveTemplateResult {
    return {
      updatedEditorState: editorState,
      newPath: target,
    }
  }
  let newIndex: number = 0
  let newPath: ElementPath | null = null
  let flexContextChanged: boolean = false

  const targetID = EP.toUid(target)
  if (newParentPath == null) {
    // TODO Scene Implementation
    return noChanges()
  } else {
    return withUnderlyingTargetFromEditorState(
      target,
      editorState,
      noChanges(),
      (underlyingElementSuccess, underlyingElement, underlyingTarget, underlyingFilePath) => {
        return withUnderlyingTargetFromEditorState(
          newParentPath,
          editorState,
          noChanges(),
          (
            newParentSuccess,
            underlyingNewParentElement,
            underlyingNewParentPath,
            underlyingNewParentFilePath,
          ) => {
            const utopiaComponentsIncludingScenes =
              getUtopiaJSXComponentsFromSuccess(newParentSuccess)
            const {
              components: withLayoutUpdatedForNewContext,
              componentMetadata: withMetadataUpdatedForNewContext,
              didSwitch,
              toast,
            } = maybeSwitchLayoutProps(
              target,
              originalPath,
              newParentPath,
              componentMetadata,
              componentMetadata,
              utopiaComponentsIncludingScenes,
              parentFrame,
              newParentLayoutSystem,
              newParentMainAxis,
              styleStringInArray,
              editorState.allElementProps,
              editorState.elementPathTree,
            )
            const updatedUnderlyingElement = findElementAtPath(
              underlyingTarget,
              withLayoutUpdatedForNewContext,
            )
            if (updatedUnderlyingElement == null) {
              return noChanges()
            } else {
              let workingEditorState: EditorState = editorState

              let updatedUtopiaComponents: Array<UtopiaJSXComponent> =
                withLayoutUpdatedForNewContext

              flexContextChanged = flexContextChanged || didSwitch

              // Remove and then insert again at the new location.
              let detailsOfUpdate: string | null = null
              workingEditorState = modifyParseSuccessAtPath(
                underlyingNewParentFilePath,
                workingEditorState,
                (workingSuccess) => {
                  updatedUtopiaComponents = removeElementAtPath(
                    underlyingTarget,
                    updatedUtopiaComponents,
                  )

                  const insertionPath = getInsertionPathWithSlotBehavior(
                    newParentPath,
                    workingEditorState.projectContents,
                    workingEditorState.nodeModules.files,
                    workingEditorState.canvas.openFile?.filename ?? null,
                    workingEditorState.jsxMetadata,
                    workingEditorState.elementPathTree,
                  )

                  if (insertionPath == null) {
                    return workingSuccess
                  }

                  const insertResult = insertJSXElementChild(
                    workingEditorState.projectContents,
                    insertionPath,
                    updatedUnderlyingElement,
                    updatedUtopiaComponents,
                    indexPosition,
                  )
                  updatedUtopiaComponents = insertResult.components
                  detailsOfUpdate = insertResult.insertionDetails

                  return {
                    ...workingSuccess,
                    imports: mergeImports(
                      underlyingFilePath,
                      underlyingElementSuccess.imports,
                      insertResult.importsToAdd,
                    ),
                    topLevelElements: applyUtopiaJSXComponentsChanges(
                      workingSuccess.topLevelElements,
                      updatedUtopiaComponents,
                    ),
                  }
                },
              )
              workingEditorState = includeToast(detailsOfUpdate, workingEditorState)

              // Validate the result of the re-insertion.
              if (newParentPath == null) {
                newIndex = updatedUtopiaComponents.findIndex(
                  (exported) => exported.rootElement === updatedUnderlyingElement,
                )
                if (newIndex === -1) {
                  throw new Error('Invalid root element index.')
                }
              } else {
                // Can't rely on underlyingNewParentElement as that will now be out of date.
                const updatedUnderlyingNewParentElement = forceNotNull(
                  'Element should exist',
                  findJSXElementAtPath(underlyingNewParentPath, updatedUtopiaComponents),
                )
                newIndex =
                  updatedUnderlyingNewParentElement.children.indexOf(updatedUnderlyingElement)
                if (newIndex === -1) {
                  throw new Error('Invalid child element index.')
                }
              }

              newPath = EP.appendToPath(newParentPath, targetID)

              let updatedComponentMetadata: ElementInstanceMetadataMap =
                withMetadataUpdatedForNewContext
              // Need to make these changes ahead of updating the frame.
              const elementMetadata = MetadataUtils.findElementByElementPath(
                updatedComponentMetadata,
                target,
              )

              if (elementMetadata != null) {
                const elementMetadataWithNewPath: ElementInstanceMetadata = {
                  ...elementMetadata,
                  elementPath: newPath,
                }

                updatedComponentMetadata = MetadataUtils.removeElementMetadataChild(
                  target,
                  updatedComponentMetadata,
                )

                updatedComponentMetadata = MetadataUtils.insertElementMetadataChild(
                  newParentPath,
                  elementMetadataWithNewPath,
                  updatedComponentMetadata,
                )
              }
              workingEditorState.jsxMetadata = updatedComponentMetadata

              if (
                newFrame !== SkipFrameChange &&
                newFrame != null &&
                newPath != null &&
                !flexContextChanged
              ) {
                const isParentFlex =
                  MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
                    originalPath,
                    componentMetadata,
                  )
                const frameChanges: Array<PinOrFlexFrameChange> = [
                  getFrameChange(newPath, newFrame, isParentFlex),
                ]

                workingEditorState = updateFramesOfScenesAndComponents(
                  workingEditorState,
                  frameChanges,
                  parentFrame,
                )
              }

              const newSelectedViews = selectedViews.map((v) => {
                if (EP.pathsEqual(v, target)) {
                  return newPath
                } else {
                  return v
                }
              })

              const newHighlightedViews =
                newParentPath == null
                  ? highlightedViews.map((t) => (EP.pathsEqual(t, target) ? newPath : t))
                  : [newParentPath]

              const updatedEditorState: EditorState = {
                ...workingEditorState,
                selectedViews: Utils.stripNulls(newSelectedViews),
                highlightedViews: Utils.stripNulls(newHighlightedViews),
                toasts: uniqToasts([...workingEditorState.toasts, ...toast]),
              }

              return {
                updatedEditorState: updatedEditorState,
                newPath: newPath,
              }
            }
          },
        )
      },
    )
  }
}

export function getCanvasOffset(
  previousOffset: CanvasPoint,
  previousScale: number,
  scale: number,
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  focusPoint: CanvasPoint | null,
  isFirstLoad: boolean,
): CanvasPoint {
  // TODO HACK getting the canvas element's size here, this should probably go away once we manage the size of the panes
  const canvasDiv = document.getElementById('canvas-root')
  const pinFocusPointOnScreen = focusPoint != null
  const canvasDivSize = canvasDiv == null ? null : canvasDiv.getBoundingClientRect()
  if (canvasDivSize != null && canvasDivSize.width !== 0 && canvasDivSize.height !== 0) {
    const zoomFocusPoint =
      focusPoint ??
      focusPointForZoom(
        selectedViews,
        scale,
        previousScale,
        componentMetadata,
        previousOffset,
        canvasDivSize,
      )
    const centerOffset = Utils.scaleVector(
      {
        x: canvasDivSize.width / 2,
        y: canvasDivSize.height / 2,
      } as CanvasPoint,
      1 / scale,
    )
    const offsetFocusPoint = Utils.pointDifference(centerOffset, zoomFocusPoint)
    if (!pinFocusPointOnScreen) {
      return Utils.negate(offsetFocusPoint)
    } else {
      // we need to shift the new offset by the distance of the new canvas center and the focus point
      const currentVisibleCenter = Utils.offsetPoint(
        Utils.scaleVector(
          {
            x: canvasDivSize.width / 2,
            y: canvasDivSize.height / 2,
          } as CanvasPoint,
          1 / previousScale,
        ),
        Utils.negate(previousOffset),
      )
      const focusPointFromVisibleCenter = Utils.pointDifference(
        currentVisibleCenter,
        zoomFocusPoint,
      )
      const differenceVectorInNewScale = Utils.scaleVector(
        focusPointFromVisibleCenter,
        previousScale / scale,
      )
      return Utils.negate(Utils.pointDifference(differenceVectorInNewScale, offsetFocusPoint))
    }
  } else {
    if (isFirstLoad) {
      // Attempt to focus on something to prevent the user opening up a barren wasteland of a canvas
      const defaultOffset = { x: 20, y: 60 }
      const selectedView = selectedViews[0]
      if (selectedView == null) {
        return defaultOffset as CanvasPoint
      } else {
        const frame = MetadataUtils.getFrameInCanvasCoords(selectedView, componentMetadata)

        if (frame == null || isInfinityRectangle(frame)) {
          return defaultOffset as CanvasPoint
        } else {
          return {
            x: frame.x + defaultOffset.x,
            y: frame.y + defaultOffset.y,
          } as CanvasPoint
        }
      }
    } else {
      return previousOffset
    }
  }
}

function focusPointForZoom(
  selectedViews: Array<ElementPath>,
  scale: number,
  previousScale: number,
  componentMetadata: ElementInstanceMetadataMap,
  canvasOffset: CanvasPoint,
  canvasDivSize: ClientRect,
): CanvasPoint {
  if (selectedViews.length > 0) {
    const accumulatedPoint = selectedViews.reduce((working, selectedView) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(selectedView, componentMetadata)

      if (frame == null || isInfinityRectangle(frame)) {
        return working
      } else {
        return {
          x: working.x + (frame.x + frame.width / 2),
          y: working.y + (frame.y + frame.height / 2),
        }
      }
    }, Utils.zeroPoint)
    return {
      x: accumulatedPoint.x / selectedViews.length,
      y: accumulatedPoint.y / selectedViews.length,
    } as CanvasPoint
  } else {
    return {
      x: ((canvasDivSize.width / 2) * 1) / previousScale - canvasOffset.x,
      y: ((canvasDivSize.height / 2) * 1) / previousScale - canvasOffset.y,
    } as CanvasPoint
  }
}

export interface DuplicateResult {
  updatedEditorState: EditorState
  originalFrames: Array<CanvasFrameAndTarget> | null
}

export function duplicate(
  paths: Array<ElementPath>,
  newParentPath: ElementPath | null,
  editor: EditorState,
  duplicateNewUIDsInjected: ReadonlyArray<DuplicateNewUID> = [],
  anchor: 'before' | 'after' = 'after',
): DuplicateResult | null {
  const duplicateNewUIDs: ReadonlyArray<DuplicateNewUID> = duplicateNewUIDsInjected
  let newOriginalFrames: Array<CanvasFrameAndTarget> | null = null

  let newSelectedViews: Array<ElementPath> = []
  let workingEditorState: EditorState = editor
  const existingIDsMutable = new Set(getAllUniqueUids(workingEditorState.projectContents).allIDs)
  for (const path of paths) {
    let metadataUpdate: (metadata: ElementInstanceMetadataMap) => ElementInstanceMetadataMap = (
      metadata,
    ) => metadata
    let detailsOfUpdate: string | null = null
    workingEditorState = modifyUnderlyingElementForOpenFile(
      path,
      workingEditorState,
      (elem) => elem,
      (success, underlyingInstancePath, underlyingFilePath) => {
        let utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
        let newElement: JSXElementChild | null = null
        let jsxElement: JSXElementChild | null = findElementAtPath(
          underlyingInstancePath,
          utopiaComponents,
        )
        const elementIndex = getIndexInParent(
          success.topLevelElements,
          EP.dynamicPathToStaticPath(path),
        )
        let uid: string
        if (jsxElement == null) {
          console.warn(`Could not find element ${EP.toVarSafeComponentId(path)}`)
          return success
        } else {
          const duplicateNewUID: DuplicateNewUID | undefined = duplicateNewUIDs.find((entry) =>
            EP.pathsEqual(entry.originalPath, path),
          )
          if (duplicateNewUID === undefined) {
            newElement = guaranteeUniqueUids([jsxElement], existingIDsMutable).value[0]
            uid = getUtopiaID(newElement)
          } else {
            // Helps to keep the model consistent because otherwise the dom walker
            // goes into a frenzy.
            newElement = setUtopiaID(jsxElement, duplicateNewUID.newUID)
            newElement = guaranteeUniqueUids([newElement], existingIDsMutable).value[0]
            uid = duplicateNewUID.newUID
          }
          let newPath: ElementPath
          if (newParentPath == null) {
            const storyboardUID = Utils.forceNotNull(
              'Could not find storyboard element',
              getStoryboardUID(utopiaComponents),
            )
            newPath = EP.elementPath([[storyboardUID, uid]])
          } else {
            newPath = EP.appendToPath(newParentPath, uid)
          }
          // Update the original frames to be the duplicate ones.
          if (newOriginalFrames != null && newPath != null) {
            newOriginalFrames = newOriginalFrames.map((originalFrame) => {
              if (EP.pathsEqual(originalFrame.target, path)) {
                return {
                  frame: originalFrame.frame,
                  target: newPath as ElementPath,
                }
              } else {
                return originalFrame
              }
            })
          }

          // Where the parent is a different component to the element being duplicated.
          const duplicatingComponentRootElement = EP.isRootElementOfInstance(path)

          if (newElement == null || duplicatingComponentRootElement) {
            console.warn(`Could not duplicate ${EP.toVarSafeComponentId(path)}`)
            return success
          } else {
            const position = () => {
              switch (anchor) {
                case 'before':
                  return before(elementIndex)
                case 'after':
                  return after(elementIndex)
                default:
                  assertNever(anchor)
              }
            }

            const conditionalCase = getConditionalCaseCorrespondingToBranchPath(
              path,
              editor.jsxMetadata,
            )

            if (conditionalCase != null && isEmptyConditionalBranch(path, editor.jsxMetadata)) {
              // can't duplicate empty conditional branch
              return success
            }

            const insertionPath =
              conditionalCase != null
                ? conditionalClauseInsertionPath(
                    EP.parentPath(path),
                    conditionalCase,
                    'wrap-with-fragment',
                  )
                : childInsertionPath(EP.parentPath(newPath))

            const insertResult = insertElementAtPath(
              editor.projectContents,
              insertionPath,
              newElement,
              utopiaComponents,
              position(),
            )

            utopiaComponents = insertResult.components
            detailsOfUpdate = insertResult.insertionDetails

            newSelectedViews.push(newPath)
            // duplicating and inserting the metadata to ensure we're not working with stale metadata
            // this is used for drag + duplicate on the canvas
            const nonNullNewElement: JSXElementChild = newElement
            metadataUpdate = (metadata) =>
              MetadataUtils.duplicateElementMetadataAtPath(
                path,
                newPath,
                right(nonNullNewElement),
                metadata,
              )

            return {
              ...success,
              imports: mergeImports(underlyingFilePath, success.imports, insertResult.importsToAdd),
              topLevelElements: applyUtopiaJSXComponentsChanges(
                success.topLevelElements,
                utopiaComponents,
              ),
            }
          }
        }
      },
    )
    workingEditorState = includeToast(detailsOfUpdate, workingEditorState)
    workingEditorState = {
      ...workingEditorState,
      jsxMetadata: metadataUpdate(workingEditorState.jsxMetadata),
    }
  }

  return {
    updatedEditorState: {
      ...workingEditorState,
      selectedViews: newSelectedViews,
    },
    originalFrames: newOriginalFrames,
  }
}

export function reorderComponent(
  projectContents: ProjectContentTreeRoot,
  components: Array<UtopiaJSXComponent>,
  target: ElementPath,
  indexPosition: IndexPosition,
): Array<UtopiaJSXComponent> {
  let workingComponents = [...components]

  const jsxElement = findElementAtPath(target, workingComponents)
  const parentPath = EP.parentPath(target)
  const parentElement = findJSXElementLikeAtPath(parentPath, workingComponents)

  if (jsxElement != null && parentElement != null) {
    const indexOfRemovedElement = parentElement.children.indexOf(jsxElement)
    if (indexOfRemovedElement < 0) {
      throw new Error(`Unable to determine old element index.`)
    }
    workingComponents = removeElementAtPath(target, workingComponents)
    const adjustedIndexPosition = shiftIndexPositionForRemovedElement(
      indexPosition,
      indexOfRemovedElement,
    )

    workingComponents = insertElementAtPath(
      projectContents,
      childInsertionPath(parentPath),
      jsxElement,
      workingComponents,
      adjustedIndexPosition,
    ).components
  }

  return workingComponents
}

interface GetParseSuccessResult {
  topLevelElements: Array<TopLevelElement>
  imports: Imports
  jsxFactoryFunction: string | null
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null
  highlightBounds: HighlightBoundsForUids | null
  exportsDetail: ExportsDetail
}

const EmptyResult: GetParseSuccessResult = {
  topLevelElements: [],
  imports: {},
  jsxFactoryFunction: null,
  combinedTopLevelArbitraryBlock: null,
  highlightBounds: null,
  exportsDetail: [],
}

export function getParseSuccessForFilePath(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
): GetParseSuccessResult {
  const projectFile = getContentsTreeFileFromString(projectContents, filePath)
  if (
    projectFile != null &&
    isTextFile(projectFile) &&
    isParseSuccess(projectFile.fileContents.parsed)
  ) {
    const parseSuccess = projectFile.fileContents.parsed
    return {
      topLevelElements: parseSuccess.topLevelElements,
      imports: parseSuccess.imports,
      jsxFactoryFunction: parseSuccess.jsxFactoryFunction,
      combinedTopLevelArbitraryBlock: parseSuccess.combinedTopLevelArbitraryBlock,
      highlightBounds: parseSuccess.highlightBounds,
      exportsDetail: parseSuccess.exportsDetail,
    }
  } else {
    return EmptyResult
  }
}

export function getValidElementPaths(
  focusedElementPath: ElementPath | null,
  topLevelElementName: string,
  instancePath: ElementPath,
  projectContents: ProjectContentTreeRoot,
  filePath: string,
  resolve: (importOrigin: string, toImport: string) => Either<string, string>,
): Array<ElementPath> {
  const { topLevelElements, imports } = getParseSuccessForFilePath(filePath, projectContents)
  const importSource = importedFromWhere(filePath, topLevelElementName, topLevelElements, imports)
  if (importSource != null) {
    let originTopLevelName = getTopLevelName(importSource, topLevelElementName)
    const resolvedImportSource = resolve(filePath, importSource.filePath)
    if (isRight(resolvedImportSource)) {
      const resolvedFilePath = resolvedImportSource.value
      const { topLevelElements: resolvedTopLevelElements, exportsDetail } =
        getParseSuccessForFilePath(resolvedFilePath, projectContents)
      // Handle default exports as they may actually be named.
      if (originTopLevelName == null) {
        for (const exportDetail of exportsDetail) {
          if (exportDetail.type === 'EXPORT_DEFAULT_FUNCTION_OR_CLASS') {
            originTopLevelName = exportDetail.name
          }
        }
      }
      const topLevelElement = resolvedTopLevelElements.find(
        (element): element is UtopiaJSXComponent => {
          return isUtopiaJSXComponent(element) && element.name === originTopLevelName
        },
      )
      if (topLevelElement != null) {
        return getValidElementPathsFromElement(
          focusedElementPath,
          topLevelElement.rootElement,
          instancePath,
          projectContents,
          resolvedFilePath,
          filePath,
          false,
          true,
          resolve,
        )
      }
    }
  }
  return []
}

function getValidElementPathsFromElement(
  focusedElementPath: ElementPath | null,
  element: JSXElementChild,
  parentPath: ElementPath,
  projectContents: ProjectContentTreeRoot,
  filePath: string,
  uiFilePath: string,
  isOnlyChildOfScene: boolean,
  parentIsInstance: boolean,
  resolve: (importOrigin: string, toImport: string) => Either<string, string>,
): Array<ElementPath> {
  if (isJSXElementLike(element)) {
    const isScene = isSceneElement(element, filePath, projectContents)
    const isSceneWithOneChild = isScene && element.children.length === 1
    const uid = getUtopiaID(element)
    const path = parentIsInstance
      ? EP.appendNewElementPath(parentPath, uid)
      : EP.appendToPath(parentPath, uid)
    let paths = [path]
    fastForEach(element.children, (c) =>
      paths.push(
        ...getValidElementPathsFromElement(
          focusedElementPath,
          c,
          path,
          projectContents,
          filePath,
          uiFilePath,
          isSceneWithOneChild,
          false,
          resolve,
        ),
      ),
    )

    const name = isJSXElement(element) ? getJSXElementNameAsString(element.name) : 'Fragment'
    const lastElementPathPart = EP.lastElementPathForPath(path)
    const matchingFocusedPathPart =
      focusedElementPath == null || lastElementPathPart == null
        ? null
        : EP.pathUpToElementPath(focusedElementPath, lastElementPathPart, 'static-path')

    const isFocused = isOnlyChildOfScene || matchingFocusedPathPart != null
    if (isFocused) {
      paths.push(
        ...getValidElementPaths(
          focusedElementPath,
          name,
          matchingFocusedPathPart ?? path,
          projectContents,
          filePath,
          resolve,
        ),
      )
    }

    return paths
  } else if (isJSExpressionOtherJavaScript(element)) {
    // FIXME: From investigation of https://github.com/concrete-utopia/utopia/issues/1137
    // The paths this will generate will only be correct if the elements from `elementsWithin`
    // are used at the same level at which they're defined.
    // This will work fine:
    // export var SameFileApp = (props) => {
    //   const AppAsVariable = App
    //   return <AppAsVariable />
    // }
    // This will not:
    // export var SameFileApp = (props) => {
    //   const AppAsVariable = App
    //   return <div data-uid='same-file-app-div' style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}>
    //     <AppAsVariable />
    //   </div>
    // }
    let paths: Array<ElementPath> = []
    fastForEach(Object.values(element.elementsWithin), (e) =>
      // We explicitly prevent auto-focusing generated elements here, because to support it would
      // require using the elementPathTree to determine how many children of a scene were actually
      // generated, creating a chicken and egg situation.
      paths.push(
        ...getValidElementPathsFromElement(
          focusedElementPath,
          e,
          parentPath,
          projectContents,
          filePath,
          uiFilePath,
          false,
          parentIsInstance,
          resolve,
        ),
      ),
    )
    return paths
  } else if (isJSXConditionalExpression(element)) {
    const uid = getUtopiaID(element)
    const path = parentIsInstance
      ? EP.appendNewElementPath(parentPath, uid)
      : EP.appendToPath(parentPath, uid)
    let paths = [path]
    fastForEach([element.whenTrue, element.whenFalse], (e) => {
      paths.push(
        ...getValidElementPathsFromElement(
          focusedElementPath,
          e,
          path,
          projectContents,
          filePath,
          uiFilePath,
          false,
          false,
          resolve,
        ),
      )
    })
    return paths
  } else {
    return []
  }
}

export const MoveIntoDragThreshold = 2

function getObservableValueForLayoutProp(
  elementMetadata: ElementInstanceMetadata | null,
  layoutProp: LayoutTargetableProp,
  elementProps: ElementProps,
): unknown {
  if (elementMetadata == null) {
    return null
  } else {
    const localFrame = nullIfInfinity(elementMetadata.localFrame)

    switch (layoutProp) {
      case 'width':
      case 'minWidth':
      case 'maxWidth':
        return localFrame?.width
      case 'height':
      case 'minHeight':
      case 'maxHeight':
        return localFrame?.height
      case 'flexBasis':
      case 'flexGrow':
      case 'flexShrink':
        const path = stylePropPathMappingFn(layoutProp, styleStringInArray)
        return Utils.pathOr(null, PP.getElements(path), elementProps)
      case 'marginTop':
        return elementMetadata.specialSizeMeasurements.margin.top
      case 'marginBottom':
        return elementMetadata.specialSizeMeasurements.margin.bottom
      case 'marginLeft':
        return elementMetadata.specialSizeMeasurements.margin.left
      case 'marginRight':
        return elementMetadata.specialSizeMeasurements.margin.right
      case 'left':
        return localFrame?.x
      case 'top':
        return localFrame?.y
      case 'right':
        return localFrame == null ||
          elementMetadata.specialSizeMeasurements.coordinateSystemBounds == null
          ? null
          : elementMetadata.specialSizeMeasurements.coordinateSystemBounds.width -
              (localFrame.width + localFrame.x)
      case 'bottom':
        return localFrame == null ||
          elementMetadata.specialSizeMeasurements.coordinateSystemBounds == null
          ? null
          : elementMetadata.specialSizeMeasurements.coordinateSystemBounds.height -
              (localFrame.height + localFrame.y)
      default:
        const _exhaustiveCheck: never = layoutProp
        throw new Error(`Unhandled prop ${JSON.stringify(layoutProp)}`)
    }
  }
}

export async function pickColorWithEyeDropper(): Promise<{ sRGBHex: string }> {
  const EyeDropper = window.EyeDropper
  if (EyeDropper == null) {
    throw new Error('EyeDropper API not supported')
  }
  const result: any = await new EyeDropper().open()
  const sRGBHex = result['sRGBHex']
  if (typeof sRGBHex === 'string') {
    return { sRGBHex }
  }
  throw new Error('No result returned')
}

export function elementHasOnlyTextChildren(element: ElementInstanceMetadata): boolean {
  const textChildren = foldEither(
    () => [],
    (e) => (e.type === 'JSX_ELEMENT' ? e.children : []),
    element.element,
  )
  const allChildrenText = textChildren.every(
    (c) =>
      c.type === 'JSX_TEXT_BLOCK' ||
      (c.type === 'JSX_ELEMENT' && jsxElementNameEquals(c.name, jsxElementName('br', []))),
  )
  const hasTextChildren = textChildren.length > 0
  return hasTextChildren && allChildrenText
}

export function isEntryAConditionalSlot(
  metadata: ElementInstanceMetadataMap,
  navigatorEntry: NavigatorEntry,
): boolean {
  const parentPath = EP.parentPath(navigatorEntry.elementPath)
  const parentElement = MetadataUtils.findElementByElementPath(metadata, parentPath)

  if (parentElement == null) {
    return false
  }

  const isParentConditional =
    parentElement != null &&
    isRight(parentElement.element) &&
    isJSXConditionalExpression(parentElement.element.value)

  const isNullValue =
    isSyntheticNavigatorEntry(navigatorEntry) &&
    isNullJSXAttributeValue(navigatorEntry.childOrAttribute)

  return isParentConditional && isNullValue
}

export function shouldShowErrorOverlay(
  errorRecords: ErrorMessage[],
  overlayErrors: OverlayError[],
): boolean {
  return errorRecords.length > 0 || overlayErrors.length > 0
}
