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
  DragState,
  DuplicateNewUID,
  EdgePosition,
  flexResizeChange,
  MoveDragState,
  oppositeEdgePositionPart,
  DragStatePositions,
  pinFrameChange,
  PinOrFlexFrameChange,
  ResizeDragState,
  singleResizeChange,
  ResizeDragStatePropertyChange,
  CreateDragState,
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

export function getOriginalFrames(
  selectedViews: Array<ElementPath>,
  componentMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
): Array<OriginalCanvasAndLocalFrame> {
  let originalFrames: Array<OriginalCanvasAndLocalFrame> = []
  function includeChildren(view: ElementPath): Array<ElementPath> {
    return [
      view,
      ...MetadataUtils.getChildrenOrdered(componentMetadata, pathTrees, view).map(
        (child) => child.elementPath,
      ),
    ]
  }
  Utils.fastForEach(
    extendSelectedViewsForInteraction(selectedViews, componentMetadata),
    (selectedView) => {
      const allPaths = Utils.flatMapArray(includeChildren, EP.allPathsForLastPart(selectedView))
      Utils.fastForEach(allPaths, (path) => {
        let alreadyAdded = false
        Utils.fastForEach(originalFrames, (originalFrame) => {
          if (EP.pathsEqual(originalFrame.target, path)) {
            alreadyAdded = true
          }
        })
        if (!alreadyAdded) {
          // TODO Scene Implementation - this should only be one call
          const localFrame = MetadataUtils.getFrame(path, componentMetadata)
          const globalFrame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
          if (
            localFrame != null &&
            globalFrame != null &&
            isFiniteRectangle(localFrame) &&
            isFiniteRectangle(globalFrame)
          ) {
            // Remove the ancestor frames if the immediate ones are groups.
            let workingFrame: CanvasRectangle | null = canvasRectangle(localFrame)

            const local = localRectangle(workingFrame)
            originalFrames.push({
              target: path,
              frame: Utils.defaultIfNull<LocalRectangle | undefined>(undefined, local),
              canvasFrame: globalFrame,
            })
          }
        }
      })
    },
  )
  return originalFrames
}

export function getOriginalCanvasFrames(
  selectedViews: Array<ElementPath>,
  componentMetadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
): Array<CanvasFrameAndTarget> {
  const originalFrames: Array<CanvasFrameAndTarget> = []
  function includeChildren(view: ElementPath): Array<ElementPath> {
    return [
      view,
      ...MetadataUtils.getChildrenOrdered(componentMetadata, pathTrees, view).map(
        (child) => child.elementPath,
      ),
    ]
  }
  Utils.fastForEach(selectedViews, (selectedView) => {
    const selectedAndChildren = Utils.flatMapArray(
      includeChildren,
      EP.allPathsForLastPart(selectedView),
    )
    const includingParents = [...selectedAndChildren, ...selectedAndChildren.map(EP.parentPath)]
    const allPaths = uniqBy(Utils.stripNulls(includingParents), EP.pathsEqual)
    Utils.fastForEach(allPaths, (path) => {
      let alreadyAdded = false
      Utils.fastForEach(originalFrames, (originalFrame) => {
        if (EP.pathsEqual(originalFrame.target, path)) {
          alreadyAdded = true
        }
      })
      if (!alreadyAdded) {
        const frame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
        if (frame != null && isFiniteRectangle(frame)) {
          originalFrames.push({
            target: path,
            frame: frame,
          })
        }
      }
    })
  })
  return originalFrames
}

function applyTransientFilesState(
  producedTransientFilesState: TransientFilesState | null,
  toastsToAdd: ReadonlyArray<Notice>,
  result: EditorState,
): EditorState {
  let workingState = result
  if (producedTransientFilesState != null) {
    for (const filePath of Object.keys(producedTransientFilesState)) {
      const producedTransientFileState = producedTransientFilesState[filePath]
      workingState = modifyParseSuccessAtPath(filePath, workingState, (success) => {
        let parseSuccessResult: ParseSuccess = {
          ...success,
          imports: producedTransientFileState.imports,
          topLevelElements: producedTransientFileState.topLevelElementsIncludingScenes,
        }
        return parseSuccessResult
      })
    }
  }

  return {
    ...workingState,
    toasts: uniqToasts([...workingState.toasts, ...toastsToAdd]),
  }
}

export function createOrUpdateDragState(
  dispatch: EditorDispatch,
  model: EditorState,
  action: CreateDragState,
): EditorState {
  return {
    ...model,
    canvas: {
      ...model.canvas,
      dragState: action.dragState,
    },
  }
}

export function clearDragState(
  model: EditorState,
  derived: DerivedState,
  applyChanges: boolean,
): EditorState {
  let result: EditorState = model
  if (
    applyChanges &&
    result.canvas.dragState != null &&
    getDragStateDrag(result.canvas.dragState, result.canvas.resizeOptions) != null
  ) {
    const producedTransientCanvasState = produceCanvasTransientState(
      derived.transientState.selectedViews,
      result,
      false,
    )

    const producedTransientFilesState = producedTransientCanvasState.filesState
    result = applyTransientFilesState(
      producedTransientFilesState,
      producedTransientCanvasState.toastsToApply,
      result,
    )
  }

  return {
    ...result,
    canvas: {
      ...result.canvas,
      dragState: null,
    },
    selectedViews: applyChanges ? derived.transientState.selectedViews : result.selectedViews,
  }
}

export function canvasFrameToNormalisedFrame(frame: CanvasRectangle): NormalisedFrame {
  const { x, y, width, height } = frame
  return { left: x, top: y, width, height }
}

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

export function cssNumberAsNumberIfPossible(
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

export function referenceParentValueForProp(prop: LayoutPinnedProp, parentSize: Size): number {
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

export function valueToUseForPin(
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

export function getOriginalFrameInCanvasCoords(
  originalFrames: Array<OriginalCanvasAndLocalFrame>,
  target: ElementPath,
): CanvasRectangle | null {
  for (const originalFrame of originalFrames ?? []) {
    if (EP.pathsEqual(target, originalFrame.target)) {
      if (originalFrame.canvasFrame != null) {
        return originalFrame.canvasFrame
      }
    }
  }
  return null
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

function getTargetableProp(resizeOptions: ResizeOptions): LayoutTargetableProp | undefined {
  return resizeOptions.propertyTargetOptions[resizeOptions.propertyTargetSelectedIndex]
}

function findResizePropertyChange(
  dragState: ResizeDragState,
  resizeOptions: ResizeOptions,
): ResizeDragStatePropertyChange | undefined {
  const resizeProp: LayoutTargetableProp | undefined = getTargetableProp(resizeOptions)
  return dragState.properties.find((prop) => prop.targetProperty === resizeProp)
}

function calculateDraggedRectangle(
  editor: EditorState,
  dragState: ResizeDragState,
): CanvasRectangle {
  const originalSize = dragState.originalSize
  const resizeOptions = editor.canvas.resizeOptions

  const propertyChange = findResizePropertyChange(dragState, resizeOptions)
  if (propertyChange == null) {
    return originalSize
  } else {
    // for center based resize, we need to calculate with double deltas
    // for now, because scale is not a first-class citizen, we know that CanvasVector and LocalVector have the same dimensions
    // this will break with the introduction of scale into the coordinate systems
    const deltaScale = propertyChange.centerBasedResize ? 2 : 1
    let delta: CanvasVector = canvasPoint({ x: 0, y: 0 })
    const drag = getDragStateDrag(dragState, editor.canvas.resizeOptions)
    if (drag != null) {
      delta = Utils.scaleVector(
        Utils.scalePoint(drag, dragState.enabledDirection as CanvasVector),
        deltaScale,
      )
    }
    const startingCorner: EdgePosition = {
      x: 1 - dragState.edgePosition.x,
      y: 1 - dragState.edgePosition.y,
    } as EdgePosition
    const startingPoint = pickPointOnRect(originalSize, startingCorner)
    const originalCenter = Utils.getRectCenter(originalSize)
    const draggedCorner = pickPointOnRect(originalSize, dragState.edgePosition)

    const newCorner = Utils.offsetPoint(draggedCorner, delta)
    const snappedNewCorner = Utils.roundPointTo(
      snapPoint(
        editor.selectedViews,
        editor.jsxMetadata,
        editor.canvas.scale,
        newCorner,
        propertyChange.enableSnapping,
        propertyChange.keepAspectRatio,
        startingPoint,
        draggedCorner,
        startingCorner,
        editor.allElementProps,
        editor.elementPathTree,
      ).snappedPointOnCanvas,
      0,
    )
    const newSizeVector = Utils.pointDifference(startingPoint, snappedNewCorner)
    const newRectangle = propertyChange.centerBasedResize
      ? Utils.rectFromPointVector(originalCenter, Utils.scaleVector(newSizeVector, 0.5), true)
      : Utils.rectFromPointVector(startingPoint, newSizeVector, false)
    return newRectangle
  }
}

export function calculateNewBounds(
  editor: EditorState,
  dragState: ResizeDragState,
): CanvasRectangle {
  const originalSize = dragState.originalSize
  const aspectRatio = originalSize.width / originalSize.height
  const newRectangle = calculateDraggedRectangle(editor, dragState)
  const resizeOptions = editor.canvas.resizeOptions

  const propertyChange = findResizePropertyChange(dragState, resizeOptions)
  if (propertyChange == null) {
    return originalSize
  } else {
    // In an aspect ratio locked resize if one dimension doesn't change then neither can the other.
    // FIXME: Replace with handling for this during drag.
    /*
  if (dragState.keepAspectRatio && oldRectangle != null) {
    if (newRectangle.width === oldRectangle.width || newRectangle.height === oldRectangle.height) {
      newRectangle.width = oldRectangle.width
      newRectangle.height = oldRectangle.height
    }
  }
  */

    // At this point I do ugly things to keep side drags in line
    if (dragState.edgePosition.x === 0.5) {
      const newWidth = propertyChange.keepAspectRatio
        ? Utils.roundTo(newRectangle.height * aspectRatio)
        : originalSize.width
      newRectangle.x -= newWidth / 2
      newRectangle.width = newWidth
    }
    if (dragState.edgePosition.y === 0.5) {
      const newHeight = propertyChange.keepAspectRatio
        ? Utils.roundTo(newRectangle.width / aspectRatio)
        : originalSize.height
      newRectangle.y -= newHeight / 2
      newRectangle.height = newHeight
    }

    return newRectangle
  }
}

export function getCursorFromDragState(editorState: EditorState): CSSCursor | null {
  const dragState = editorState.canvas.dragState
  if (dragState == null) {
    return null
  } else {
    switch (dragState.type) {
      case 'MOVE_DRAG_STATE':
        if (dragState.drag == null) {
          return null
        } else {
          return CSSCursor.Move
        }
      case 'RESIZE_DRAG_STATE':
        if (isEdgePositionAHorizontalEdge(dragState.edgePosition)) {
          return CSSCursor.ResizeNS
        } else if (isEdgePositionAVerticalEdge(dragState.edgePosition)) {
          return CSSCursor.ResizeEW
        } else if (isEdgePositionACorner(dragState.edgePosition)) {
          // Slightly more complicated as we need to determine if the corner has flipped.
          let edgePosition: EdgePosition = dragState.edgePosition
          const bounds = calculateNewBounds(editorState, dragState)
          const leftEdgePastOldRightEdge =
            dragState.edgePosition.x === 0 &&
            bounds.x === dragState.originalSize.x + dragState.originalSize.width &&
            bounds.width > 0
          const rightEdgePastOldLeftEdge =
            dragState.edgePosition.x === 1 && bounds.x !== dragState.originalSize.x
          const topEdgePastOldBottomEdge =
            dragState.edgePosition.y === 0 &&
            bounds.y === dragState.originalSize.y + dragState.originalSize.height &&
            bounds.height > 0
          const bottomEdgePastOldTopEdge =
            dragState.edgePosition.y === 1 && bounds.y !== dragState.originalSize.y

          if (leftEdgePastOldRightEdge || rightEdgePastOldLeftEdge) {
            edgePosition = {
              ...edgePosition,
              x: oppositeEdgePositionPart(edgePosition.x),
            }
          }

          if (topEdgePastOldBottomEdge || bottomEdgePastOldTopEdge) {
            edgePosition = {
              ...edgePosition,
              y: oppositeEdgePositionPart(edgePosition.y),
            }
          }
          const isTopLeft = edgePosition.x === 0 && edgePosition.y === 0
          const isBottomRight = edgePosition.x === 1 && edgePosition.y === 1

          return isTopLeft || isBottomRight ? CSSCursor.ResizeNWSE : CSSCursor.ResizeNESW
        } else {
          return null
        }
      case 'INSERT_DRAG_STATE':
        return null
      default:
        const _exhaustiveCheck: never = dragState
        throw new Error(`Unhandled drag state type ${JSON.stringify(dragState)}`)
    }
  }
}

function getTransientCanvasStateFromFrameChanges(
  editorState: EditorState,
  framesAndTargets: Array<PinOrFlexFrameChange>,
  preventAnimations: boolean,
  elementsToTarget: Array<ElementPath>,
): TransientCanvasState {
  let workingEditorState: EditorState = editorState
  let successByFilename: { [filename: string]: ParseSuccess } = {}

  if (preventAnimations) {
    // We don't want animations included in the transient state, except for the case where we're about to apply that to the final state
    workingEditorState = preventAnimationsOnTargets(workingEditorState, elementsToTarget)
  }
  workingEditorState = updateFramesOfScenesAndComponents(workingEditorState, framesAndTargets, null)

  for (const frameAndTarget of framesAndTargets) {
    forUnderlyingTargetFromEditorState(
      frameAndTarget.target,
      workingEditorState,
      (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
        successByFilename[underlyingFilePath] = success
        return success
      },
    )
  }

  return transientCanvasState(
    editorState.selectedViews,
    editorState.highlightedViews,
    [],
    mapValues((success) => {
      return transientFileState(success.topLevelElements, success.imports)
    }, successByFilename),
    workingEditorState.toasts, // TODO filter for relevant toasts
  )
}

export function produceResizeCanvasTransientState(
  editorState: EditorState,
  dragState: ResizeDragState,
  preventAnimations: boolean,
): TransientCanvasState {
  const elementsToTarget = determineElementsToOperateOnForDragging(
    dragState.draggedElements,
    editorState.jsxMetadata,
    false,
    true,
  )

  const newSize = calculateNewBounds(editorState, dragState)
  let framesAndTargets: Array<PinOrFlexFrameChange> = []
  let globalFrames: Array<CanvasRectangle> = []
  Utils.fastForEach(dragState.draggedElements, (selectedView) => {
    const frame = getOriginalFrameInCanvasCoords(dragState.originalFrames, selectedView)
    if (frame != null) {
      globalFrames.push(frame)
    }
  })
  const boundingBox = Utils.boundingRectangleArray(globalFrames)
  if (boundingBox == null) {
    return transientCanvasState(
      dragState.draggedElements,
      editorState.highlightedViews,
      editorState.hoveredViews,
      null,
      [],
    )
  } else {
    Utils.fastForEach(elementsToTarget, (target) => {
      forUnderlyingTargetFromEditorState(
        target,
        editorState,
        (success, element, underlyingTarget, underlyingFilePath) => {
          const originalFrame = getOriginalFrameInCanvasCoords(dragState.originalFrames, target)
          if (originalFrame != null) {
            const newTargetFrame = Utils.transformFrameUsingBoundingBox(
              newSize,
              boundingBox,
              originalFrame,
            )
            const roundedFrame = {
              x: Math.floor(newTargetFrame.x),
              y: Math.floor(newTargetFrame.y),
              width: Math.ceil(newTargetFrame.width),
              height: Math.ceil(newTargetFrame.height),
            } as CanvasRectangle
            const isFlexContainer =
              MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
                target,
                editorState.jsxMetadata,
              )

            if (isFlexContainer) {
              for (const resizePropertyChange of dragState.properties) {
                if (resizePropertyChange.targetProperty != null) {
                  if (resizePropertyChange.drag != null) {
                    const newDelta = isTargetPropertyHorizontal(dragState.edgePosition)
                      ? resizePropertyChange.drag.x ?? 0
                      : resizePropertyChange.drag.y ?? 0
                    framesAndTargets.push(
                      flexResizeChange(target, resizePropertyChange.targetProperty, newDelta),
                    )
                  }
                }
              }
            } else {
              framesAndTargets.push(
                pinFrameChange(underlyingTarget, roundedFrame, dragState.edgePosition),
              )
            }
          }
        },
      )
    })

    return getTransientCanvasStateFromFrameChanges(
      editorState,
      framesAndTargets,
      preventAnimations,
      elementsToTarget,
    )
  }
}

export function isTargetPropertyHorizontal(edgePosition: EdgePosition): boolean {
  return edgePosition.x !== 0.5
}

export function produceResizeSingleSelectCanvasTransientState(
  editorState: EditorState,
  dragState: ResizeDragState,
  preventAnimations: boolean,
): TransientCanvasState {
  const elementsToTarget = determineElementsToOperateOnForDragging(
    dragState.draggedElements,
    editorState.jsxMetadata,
    false,
    true,
  )
  if (elementsToTarget.length !== 1) {
    return transientCanvasState(
      editorState.selectedViews,
      editorState.highlightedViews,
      editorState.hoveredViews,
      null,
      [],
    )
  }
  const elementToTarget = elementsToTarget[0]

  const newSize = calculateNewBounds(editorState, dragState)
  let framesAndTargets: Array<PinOrFlexFrameChange> = []
  let globalFrame = getOriginalFrameInCanvasCoords(dragState.originalFrames, elementToTarget)
  const originalFrame = getOriginalFrameInCanvasCoords(dragState.originalFrames, elementToTarget)
  if (originalFrame != null && globalFrame != null) {
    const nonNullGlobalFrame = globalFrame
    forUnderlyingTargetFromEditorState(
      elementToTarget,
      editorState,
      (success, element, underlyingTarget, underlyingFilePath) => {
        const newTargetFrame = Utils.transformFrameUsingBoundingBox(
          newSize,
          nonNullGlobalFrame,
          originalFrame,
        )
        const roundedFrame = {
          x: Math.floor(newTargetFrame.x),
          y: Math.floor(newTargetFrame.y),
          width: Math.ceil(newTargetFrame.width),
          height: Math.ceil(newTargetFrame.height),
        } as CanvasRectangle
        const isFlexContainer =
          MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
            elementToTarget,
            editorState.jsxMetadata,
          )
        for (const propertyChange of dragState.properties) {
          if (
            isFlexContainer ||
            dragState.edgePosition.x === 0.5 ||
            dragState.edgePosition.y === 0.5
          ) {
            if (propertyChange.targetProperty != null) {
              if (propertyChange.drag != null) {
                const newDelta = isTargetPropertyHorizontal(dragState.edgePosition)
                  ? propertyChange.drag.x ?? 0
                  : propertyChange.drag.y ?? 0

                framesAndTargets.push(
                  flexResizeChange(elementToTarget, propertyChange.targetProperty, newDelta),
                )
              }
            }
          } else {
            const edgePosition = propertyChange.centerBasedResize
              ? ({ x: 0.5, y: 0.5 } as EdgePosition)
              : dragState.edgePosition
            const sizeChange = {
              x: roundedFrame.width - originalFrame.width,
              y: roundedFrame.height - originalFrame.height,
            } as CanvasVector
            framesAndTargets.push(singleResizeChange(elementToTarget, edgePosition, sizeChange))
          }
        }
      },
    )
  }

  return getTransientCanvasStateFromFrameChanges(
    editorState,
    framesAndTargets,
    preventAnimations,
    elementsToTarget,
  )
}

export function produceCanvasTransientState(
  previousCanvasTransientSelectedViews: Array<ElementPath> | null,
  editorState: EditorState,
  preventAnimations: boolean,
): TransientCanvasState {
  const currentOpenFile = editorState.canvas.openFile?.filename
  let transientState: TransientCanvasState | null = null
  if (currentOpenFile != null) {
    const editorMode = editorState.mode
    switch (editorMode.type) {
      case 'insert':
        break
      case 'select':
        if (
          editorState.canvas.dragState != null &&
          anyDragStarted(editorState.canvas.dragState) &&
          anyDragMovement(editorState.canvas.dragState)
        ) {
          const dragState = editorState.canvas.dragState
          switch (dragState.type) {
            case 'MOVE_DRAG_STATE':
              transientState = produceMoveTransientCanvasState(
                previousCanvasTransientSelectedViews,
                editorState,
                dragState,
                preventAnimations,
              )
              break
            case 'RESIZE_DRAG_STATE':
              if (dragState.isMultiSelect) {
                transientState = produceResizeCanvasTransientState(
                  editorState,
                  dragState,
                  preventAnimations,
                )
              } else {
                transientState = produceResizeSingleSelectCanvasTransientState(
                  editorState,
                  dragState,
                  preventAnimations,
                )
              }
              break
            case 'INSERT_DRAG_STATE':
              throw new Error(`Unable to use insert drag state in select mode.`)
            default:
              const _exhaustiveCheck: never = dragState
              throw new Error(`Unhandled drag state type ${JSON.stringify(dragState)}`)
          }
        }
        break
    }

    if (transientState == null && editorState.canvas.transientProperties != null) {
      transientState = createCanvasTransientStateFromProperties(editorState)
    }
  }

  if (transientState == null) {
    return transientCanvasState(
      editorState.selectedViews,
      editorState.highlightedViews,
      editorState.hoveredViews,
      null,
      [],
    )
  } else {
    return transientState
  }
}

export function createDuplicationNewUIDsFromEditorState(
  editorState: EditorState,
): Array<DuplicateNewUID> {
  return createDuplicationNewUIDs(
    editorState.selectedViews,
    editorState.jsxMetadata,
    editorState.projectContents,
  )
}

export function createDuplicationNewUIDs(
  selectedViews: Array<ElementPath>,
  componentMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
): Array<DuplicateNewUID> {
  const targetViews = determineElementsToOperateOnForDragging(
    selectedViews,
    componentMetadata,
    true,
    false,
  )

  let existingIDs = getAllUniqueUids(projectContents).allIDs

  let result: Array<DuplicateNewUID> = []
  Utils.fastForEach(targetViews, (targetView) => {
    const newUID = generateUID(existingIDs)
    existingIDs.push(newUID)
    result.push({
      originalPath: targetView,
      newUID: newUID,
    })
  })

  return result
}

export const SkipFrameChange = 'skipFrameChange'

function getReparentTargetAtPosition(
  componentMeta: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  hiddenInstances: Array<ElementPath>,
  pointOnCanvas: CanvasPoint,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
): ElementPath | undefined {
  const allTargets = getAllTargetsAtPointAABB(
    componentMeta,
    selectedViews,
    hiddenInstances,
    'no-filter',
    pointOnCanvas,
    elementPathTree,
    allElementProps,
    true, // this is how it was historically, but I think it should be false?
  )
  // filtering for non-selected views from alltargets
  return allTargets.find((target) => selectedViews.every((view) => !EP.pathsEqual(view, target)))
}

export function getReparentTargetFromState(
  selectedViews: Array<ElementPath>,
  editorState: EditorState,
  toReparent: Array<ElementPath>,
  position: CanvasPoint,
): {
  shouldReparent: boolean
  newParent: ElementPath | null
} {
  return getReparentTarget(
    selectedViews,
    toReparent,
    editorState.jsxMetadata,
    editorState.hiddenInstances,
    position,
    editorState.projectContents,
    editorState.nodeModules.files,
    editorState.canvas.openFile?.filename,
    editorState.elementPathTree,
    editorState.allElementProps,
  )
}

export function getReparentTarget(
  selectedViews: Array<ElementPath>,
  toReparent: Array<ElementPath>,
  componentMeta: ElementInstanceMetadataMap,
  hiddenInstances: Array<ElementPath>,
  pointOnCanvas: CanvasPoint,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
): {
  shouldReparent: boolean
  newParent: ElementPath | null
} {
  const result = getReparentTargetAtPosition(
    componentMeta,
    selectedViews,
    hiddenInstances,
    pointOnCanvas,
    elementPathTree,
    allElementProps,
  )

  const possibleNewParent = result == undefined ? null : result
  const currentParents = Utils.stripNulls(
    toReparent.map((view) => MetadataUtils.getParent(componentMeta, view)),
  )
  let parentSupportsChild = true
  if (possibleNewParent == null) {
    // a null template path means Canvas, let's translate that to the storyboard component
    const storyboardComponent = getStoryboardElementPath(projectContents, openFile ?? null)
    return {
      shouldReparent: storyboardComponent != null,
      newParent: storyboardComponent,
    }
  } else {
    parentSupportsChild = MetadataUtils.targetSupportsChildren(
      projectContents,
      componentMeta,
      nodeModules,
      openFile,
      possibleNewParent,
      elementPathTree,
    )
  }
  const hasNoCurrentParentsButHasANewParent =
    currentParents.length === 0 && possibleNewParent != null
  const newParentIsAChangeFromTheExistingOnes =
    currentParents.length > 0 &&
    currentParents.every((parent) => !EP.pathsEqual(possibleNewParent, parent.elementPath))
  if (
    parentSupportsChild &&
    (hasNoCurrentParentsButHasANewParent || newParentIsAChangeFromTheExistingOnes)
  ) {
    return {
      shouldReparent: true,
      newParent: possibleNewParent,
    }
  } else {
    return {
      shouldReparent: false,
      newParent: null,
    }
  }
}

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

function preventAnimationsOnTargets(editorState: EditorState, targets: ElementPath[]): EditorState {
  let workingEditorState = editorState
  Utils.fastForEach(targets, (target) => {
    const staticPath = EP.dynamicPathToStaticPath(target)
    if (staticPath != null) {
      workingEditorState = modifyUnderlyingElementForOpenFile(
        staticPath,
        editorState,
        (underlyingElement) => {
          const styleUpdated = setJSXValuesAtPaths(underlyingElement.props, [
            {
              path: PP.create('style', 'transition'),
              value: jsExpressionValue('none', emptyComments),
            },
          ])
          return foldEither(
            () => underlyingElement,
            (updatedProps) => {
              return {
                ...underlyingElement,
                props: updatedProps,
              }
            },
            styleUpdated,
          )
        },
      )
    }
  })
  return workingEditorState
}

function produceMoveTransientCanvasState(
  previousCanvasTransientSelectedViews: Array<ElementPath> | null,
  editorState: EditorState,
  dragState: MoveDragState,
  preventAnimations: boolean,
): TransientCanvasState {
  let selectedViews: Array<ElementPath> = dragState.draggedElements
  let originalFrames: Array<CanvasFrameAndTarget> = dragState.originalFrames

  let elementsToTarget = determineElementsToOperateOnForDragging(
    selectedViews,
    editorState.jsxMetadata,
    true,
    false,
  )

  let workingEditorState: EditorState = editorState
  if (preventAnimations) {
    // We don't want animations included in the transient state, except for the case where we're about to apply that to the final state
    workingEditorState = preventAnimationsOnTargets(workingEditorState, elementsToTarget)
  }

  if (dragState.reparent) {
    const reparentTarget = getReparentTargetFromState(
      previousCanvasTransientSelectedViews ?? editorState.selectedViews,
      workingEditorState,
      elementsToTarget,
      dragState.canvasPosition,
    )

    if (reparentTarget.shouldReparent) {
      elementsToTarget = elementsToTarget.map((target) => {
        const frame = originalFrames.find((originalFrameAndTarget) => {
          return EP.pathsEqual(originalFrameAndTarget.target, target)
        })?.frame
        const reparentResult = moveTemplate(
          target,
          target,
          frame ?? null,
          { type: 'front' },
          reparentTarget.newParent,
          null,
          workingEditorState,
          workingEditorState.jsxMetadata,
          selectedViews,
          workingEditorState.highlightedViews,
          null,
          null,
        )
        selectedViews = reparentResult.updatedEditorState.selectedViews
        // As it has moved, we need to synchronise the paths.
        originalFrames = originalFrames.map((originalFrame) => {
          if (reparentResult.newPath != null && EP.pathsEqual(originalFrame.target, target)) {
            return {
              ...originalFrame,
              target: reparentResult.newPath,
            }
          } else {
            return originalFrame
          }
        })

        workingEditorState = reparentResult.updatedEditorState
        return reparentResult.newPath ?? target
      })
    }
  } else if (dragState.duplicate) {
    const parentTarget = MetadataUtils.getDuplicationParentTargets(selectedViews)
    const duplicateResult = duplicate(elementsToTarget, parentTarget, workingEditorState)
    if (duplicateResult != null) {
      workingEditorState = duplicateResult.updatedEditorState
      selectedViews = duplicateResult.updatedEditorState.selectedViews
      if (duplicateResult.originalFrames != null) {
        originalFrames = duplicateResult.originalFrames
      }
    }
  }

  const moveGuidelines = collectParentAndSiblingGuidelines(
    workingEditorState.jsxMetadata,
    workingEditorState.allElementProps,
    workingEditorState.elementPathTree,
    selectedViews,
  ).map((g) => g.guideline)

  const framesAndTargets = dragComponent(
    workingEditorState.jsxMetadata,
    workingEditorState.elementPathTree,
    selectedViews,
    originalFrames,
    moveGuidelines,
    dragState.dragSelectionBoundingBox,
    dragState.drag,
    Utils.defaultIfNull(Utils.zeroPoint as CanvasPoint, dragState.drag),
    dragState.enableSnapping,
    dragState.constrainDragAxis,
    workingEditorState.canvas.scale,
  )

  workingEditorState = updateFramesOfScenesAndComponents(workingEditorState, framesAndTargets, null)

  let transientFilesState: TransientFilesState = {}
  for (const elementToTarget of elementsToTarget) {
    forUnderlyingTargetFromEditorState(
      elementToTarget,
      workingEditorState,
      (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
        transientFilesState[underlyingFilePath] = {
          topLevelElementsIncludingScenes: success.topLevelElements,
          imports: success.imports,
        }
        return success
      },
    )
  }
  return transientCanvasState(
    selectedViews,
    workingEditorState.highlightedViews,
    workingEditorState.hoveredViews,
    transientFilesState,
    workingEditorState.toasts, // TODO Filter for relevant toasts
  )
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

export function focusPointForZoom(
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
  let duplicateNewUIDs: ReadonlyArray<DuplicateNewUID> = duplicateNewUIDsInjected
  let newOriginalFrames: Array<CanvasFrameAndTarget> | null = null
  if (
    editor.canvas.dragState != null &&
    editor.canvas.dragState.type === 'MOVE_DRAG_STATE' &&
    editor.canvas.dragState.duplicateNewUIDs != null
  ) {
    duplicateNewUIDs = editor.canvas.dragState.duplicateNewUIDs
    newOriginalFrames = editor.canvas.dragState.originalFrames
  }

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

export interface GetParseSuccessOrTransientResult {
  topLevelElements: Array<TopLevelElement>
  imports: Imports
  jsxFactoryFunction: string | null
  combinedTopLevelArbitraryBlock: ArbitraryJSBlock | null
  highlightBounds: HighlightBoundsForUids | null
  exportsDetail: ExportsDetail
}

const EmptyResult: GetParseSuccessOrTransientResult = {
  topLevelElements: [],
  imports: {},
  jsxFactoryFunction: null,
  combinedTopLevelArbitraryBlock: null,
  highlightBounds: null,
  exportsDetail: [],
}

export function getParseSuccessOrTransientForFilePath(
  filePath: string,
  projectContents: ProjectContentTreeRoot,
  transientFilesState: TransientFilesState | null,
): GetParseSuccessOrTransientResult {
  const projectFile = getContentsTreeFileFromString(projectContents, filePath)
  if (
    projectFile != null &&
    isTextFile(projectFile) &&
    isParseSuccess(projectFile.fileContents.parsed)
  ) {
    const parseSuccess = projectFile.fileContents.parsed
    const targetTransientFileState: TransientFileState | null =
      transientFilesState == null ? null : transientFilesState[filePath] ?? null
    if (targetTransientFileState == null) {
      return {
        topLevelElements: parseSuccess.topLevelElements,
        imports: parseSuccess.imports,
        jsxFactoryFunction: parseSuccess.jsxFactoryFunction,
        combinedTopLevelArbitraryBlock: parseSuccess.combinedTopLevelArbitraryBlock,
        highlightBounds: parseSuccess.highlightBounds,
        exportsDetail: parseSuccess.exportsDetail,
      }
    } else {
      return {
        topLevelElements: targetTransientFileState.topLevelElementsIncludingScenes,
        imports: targetTransientFileState.imports,
        jsxFactoryFunction: parseSuccess.jsxFactoryFunction,
        combinedTopLevelArbitraryBlock: parseSuccess.combinedTopLevelArbitraryBlock,
        highlightBounds: parseSuccess.highlightBounds,
        exportsDetail: parseSuccess.exportsDetail,
      }
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
  transientFilesState: TransientFilesState | null,
  resolve: (importOrigin: string, toImport: string) => Either<string, string>,
): Array<ElementPath> {
  const { topLevelElements, imports } = getParseSuccessOrTransientForFilePath(
    filePath,
    projectContents,
    transientFilesState,
  )
  const importSource = importedFromWhere(filePath, topLevelElementName, topLevelElements, imports)
  if (importSource != null) {
    let originTopLevelName = getTopLevelName(importSource, topLevelElementName)
    const resolvedImportSource = resolve(filePath, importSource.filePath)
    if (isRight(resolvedImportSource)) {
      const resolvedFilePath = resolvedImportSource.value
      const { topLevelElements: resolvedTopLevelElements, exportsDetail } =
        getParseSuccessOrTransientForFilePath(
          resolvedFilePath,
          projectContents,
          transientFilesState,
        )
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
          transientFilesState,
          resolve,
        )
      }
    }
  }
  return []
}

export function getValidElementPathsFromElement(
  focusedElementPath: ElementPath | null,
  element: JSXElementChild,
  parentPath: ElementPath,
  projectContents: ProjectContentTreeRoot,
  filePath: string,
  uiFilePath: string,
  isOnlyChildOfScene: boolean,
  parentIsInstance: boolean,
  transientFilesState: TransientFilesState | null,
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
          transientFilesState,
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
          transientFilesState,
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
          transientFilesState,
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
          transientFilesState,
          resolve,
        ),
      )
    })
    return paths
  } else {
    return []
  }
}

function createCanvasTransientStateFromProperties(
  editor: EditorState,
): TransientCanvasState | null {
  if (editor.canvas.transientProperties == null) {
    return null
  } else {
    const updatedEditor = Object.values(editor.canvas.transientProperties).reduce(
      (working, currentProp) => {
        return modifyUnderlyingTargetElement(
          currentProp.elementPath,
          Utils.forceNotNull('No open file found', getOpenUIJSFileKey(editor)),
          working,
          (element) => {
            if (isJSXConditionalExpression(element) || isJSXFragment(element)) {
              return element
            }
            const valuesAtPath = Object.keys(currentProp.attributesToUpdate).map((key) => {
              return {
                path: PP.fromString(key),
                value: currentProp.attributesToUpdate[key],
              }
            })
            let updatedAttributes = setJSXValuesAtPaths(element.props, valuesAtPath)
            return foldEither(
              (_) => element,
              (updatedProps) => {
                return {
                  ...element,
                  props: updatedProps,
                }
              },
              updatedAttributes,
            )
          },
        )
      },
      editor,
    )

    let transientFilesState: TransientFilesState = {}
    fastForEach(Object.values(editor.canvas.transientProperties) ?? [], (prop) => {
      forUnderlyingTargetFromEditorState(
        prop.elementPath,
        updatedEditor,
        (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
          transientFilesState[underlyingFilePath] = {
            topLevelElementsIncludingScenes: success.topLevelElements,
            imports: success.imports,
          }
          return success
        },
      )
    })
    return transientCanvasState(
      updatedEditor.selectedViews,
      updatedEditor.highlightedViews,
      updatedEditor.hoveredViews,
      transientFilesState,
      [],
    )
  }
}

export function getDragStatePositions(
  dragState: DragState | null,
  resizeOptions: ResizeOptions,
): DragStatePositions | null {
  if (dragState == null) {
    return null
  } else {
    switch (dragState.type) {
      case 'MOVE_DRAG_STATE':
      case 'INSERT_DRAG_STATE':
        return dragState
      case 'RESIZE_DRAG_STATE':
        return findResizePropertyChange(dragState, resizeOptions) ?? null
      default:
        const _exhaustiveCheck: never = dragState
        throw new Error(`Unhandled drag state type ${JSON.stringify(dragState)}`)
    }
  }
}

export function getDragStateDrag(
  dragState: DragState | null,
  resizeOptions: ResizeOptions,
): CanvasPoint | null {
  return optionalMap((positions) => positions.drag, getDragStatePositions(dragState, resizeOptions))
}

export function getDragStateStart(
  dragState: DragState | null,
  resizeOptions: ResizeOptions,
): CanvasPoint | null {
  return optionalMap(
    (positions) => positions.start,
    getDragStatePositions(dragState, resizeOptions),
  )
}

export function anyDragStarted(dragState: DragState | null): boolean {
  if (dragState == null) {
    return false
  } else {
    switch (dragState.type) {
      case 'MOVE_DRAG_STATE':
      case 'INSERT_DRAG_STATE':
        return dragState.start != null
      case 'RESIZE_DRAG_STATE':
        return dragState.properties.some((prop) => prop.start != null)
      default:
        const _exhaustiveCheck: never = dragState
        throw new Error(`Unhandled drag state type ${JSON.stringify(dragState)}`)
    }
  }
}

export function anyDragMovement(dragState: DragState | null): boolean {
  if (dragState == null) {
    return false
  } else {
    switch (dragState.type) {
      case 'MOVE_DRAG_STATE':
      case 'INSERT_DRAG_STATE':
        return dragState.drag != null
      case 'RESIZE_DRAG_STATE':
        return dragState.properties.some((prop) => prop.drag != null)
      default:
        const _exhaustiveCheck: never = dragState
        throw new Error(`Unhandled drag state type ${JSON.stringify(dragState)}`)
    }
  }
}

export function getResizeOptions(
  layoutSystem: 'flex-horizontal' | 'flex-vertical' | 'absolute' | null,
  controlDirection: 'horizontal' | 'vertical',
  edge: 'before' | 'after',
): Array<LayoutTargetableProp> {
  switch (layoutSystem) {
    case 'flex-horizontal':
      switch (controlDirection) {
        case 'horizontal':
          return ['height', 'minHeight', 'maxHeight']
        case 'vertical':
          return ['flexBasis', 'flexGrow', 'flexShrink', 'minWidth', 'maxWidth']
        default:
          const _exhaustiveCheck: never = controlDirection
          throw new Error(`Unhandled control direction ${JSON.stringify(controlDirection)}`)
      }
    case 'flex-vertical':
      switch (controlDirection) {
        case 'horizontal':
          return ['flexBasis', 'flexGrow', 'flexShrink', 'minHeight', 'maxHeight']
        case 'vertical':
          return ['width', 'minWidth', 'maxWidth']
        default:
          const _exhaustiveCheck: never = controlDirection
          throw new Error(`Unhandled control direction ${JSON.stringify(controlDirection)}`)
      }
    case 'absolute':
      switch (controlDirection) {
        case 'horizontal':
          switch (edge) {
            case 'before':
              return ['top', 'height', 'marginTop', 'minHeight', 'maxHeight']
            case 'after':
              return ['bottom', 'height', 'marginBottom', 'minHeight', 'maxHeight']
            default:
              const _exhaustiveCheck: never = edge
              throw new Error(`Unhandled control edge ${JSON.stringify(edge)}`)
          }
        case 'vertical':
          switch (edge) {
            case 'before':
              return ['left', 'width', 'marginLeft', 'minWidth', 'maxWidth']
            case 'after':
              return ['right', 'width', 'marginRight', 'minWidth', 'maxWidth']
            default:
              const _exhaustiveCheck: never = edge
              throw new Error(`Unhandled control edge ${JSON.stringify(edge)}`)
          }
        default:
          const _exhaustiveCheck: never = controlDirection
          throw new Error(`Unhandled control direction ${JSON.stringify(controlDirection)}`)
      }
    case null:
      switch (controlDirection) {
        case 'horizontal':
          switch (edge) {
            case 'before':
              return ['height', 'marginTop', 'minHeight', 'maxHeight']
            case 'after':
              return ['height', 'marginBottom', 'minHeight', 'maxHeight']
            default:
              const _exhaustiveCheck: never = edge
              throw new Error(`Unhandled control edge ${JSON.stringify(edge)}`)
          }
        case 'vertical':
          switch (edge) {
            case 'before':
              return ['width', 'marginLeft', 'minWidth', 'maxWidth']
            case 'after':
              return ['width', 'marginRight', 'minWidth', 'maxWidth']
            default:
              const _exhaustiveCheck: never = edge
              throw new Error(`Unhandled control edge ${JSON.stringify(edge)}`)
          }
        default:
          const _exhaustiveCheck: never = controlDirection
          throw new Error(`Unhandled control direction ${JSON.stringify(controlDirection)}`)
      }
    default:
      const _exhaustiveCheck: never = layoutSystem
      throw new Error(`Unhandled flex direction ${JSON.stringify(layoutSystem)}`)
  }
}

export const MoveIntoDragThreshold = 2

export function dragExceededThreshold(
  canvasPosition: CanvasPoint,
  dragStart: CanvasPoint,
): boolean {
  const xDiff = Math.abs(canvasPosition.x - dragStart.x)
  const yDiff = Math.abs(canvasPosition.y - dragStart.y)
  return xDiff > MoveIntoDragThreshold || yDiff > MoveIntoDragThreshold
}

export function getObservableValueForLayoutProp(
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
