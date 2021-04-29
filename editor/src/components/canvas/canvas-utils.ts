import * as R from 'ramda'
import {
  FramePoint,
  HorizontalFramePointsExceptSize,
  isFramePoint,
  isPercentPin,
  LayoutSystem,
  NormalisedFrame,
  valueToUseForPin,
  VerticalFramePointsExceptSize,
  VerticalFramePoints,
  HorizontalFramePoints,
  isHorizontalPoint,
  numberPartOfPin,
} from 'utopia-api'
import { FlexLayoutHelpers } from '../../core/layout/layout-helpers'
import {
  createLayoutPropertyPath,
  framePointForPinnedProp,
  LayoutPinnedProps,
  LayoutProp,
  pinnedPropForFramePoint,
  LayoutPinnedProp,
} from '../../core/layout/layout-helpers-new'
import {
  maybeSwitchLayoutProps,
  PinningAndFlexPoints,
  PinningAndFlexPointsExceptSize,
  roundJSXElementLayoutValues,
  roundAttributeLayoutValues,
} from '../../core/layout/layout-utils'
import {
  findElementAtPath,
  findJSXElementAtPath,
  MetadataUtils,
} from '../../core/model/element-metadata-utils'
import {
  isJSXElement,
  jsxAttributeValue,
  JSXElement,
  JSXElementChild,
  UtopiaJSXComponent,
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  setJSXAttributesAttribute,
} from '../../core/shared/element-template'
import {
  getAllUniqueUids,
  getUtopiaID,
  guaranteeUniqueUids,
  insertJSXElementChild,
  setUtopiaID,
  transformJSXComponentAtPath,
  findJSXElementChildAtPath,
  transformJSXComponentAtElementPath,
} from '../../core/model/element-template-utils'
import { generateUID } from '../../core/shared/uid-utils'
import {
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  ValueAtPath,
  setJSXValueAtPath,
  jsxAttributesToProps,
  jsxSimpleAttributeToValue,
  getJSXAttributeAtPath,
} from '../../core/shared/jsx-attributes'
import {
  Imports,
  InstancePath,
  isParseFailure,
  ParsedTextFile,
  ParseSuccess,
  RevisionsState,
  TemplatePath,
  importAlias,
  PropertyPath,
  foldParsedTextFile,
  textFile,
  textFileContents,
  isParseSuccess,
} from '../../core/shared/project-file-types'
import {
  applyUtopiaJSXComponentsChanges,
  getOrDefaultScenes,
  getUtopiaJSXComponentsFromSuccess,
} from '../../core/model/project-file-utils'
import { lintAndParse } from '../../core/workers/parser-printer/parser-printer'
import { defaultProject } from '../../sample-projects/sample-project-utils'
import {
  eitherToMaybe,
  flatMapEither,
  foldEither,
  forEachRight,
  isRight,
  right,
  isLeft,
} from '../../core/shared/either'
import Utils, { IndexPosition } from '../../utils/utils'
import {
  CanvasPoint,
  canvasPoint,
  CanvasRectangle,
  canvasRectangle,
  CanvasVector,
  localRectangle,
  LocalRectangle,
} from '../../core/shared/math-utils'
import { insertionSubjectIsJSXElement } from '../editor/editor-modes'
import {
  DerivedState,
  EditorState,
  getOpenUIJSFile,
  insertElementAtPath,
  modifyOpenParseSuccess,
  OriginalCanvasAndLocalFrame,
  PersistentModel,
  removeElementAtPath,
  TransientCanvasState,
  transientCanvasState,
  transientFileState,
  getStoryboardTemplatePathFromEditorState,
  addSceneToJSXComponents,
  getNumberOfScenes,
  StoryboardFilePath,
  modifyUnderlyingTarget,
  modifyParseSuccessAtPath,
  getOpenUIJSFileKey,
  withUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  TransientFilesState,
  forUnderlyingTargetFromEditorState,
  TransientFileState,
  withUnderlyingTarget,
} from '../editor/store/editor-state'
import * as Frame from '../frame'
import { getImageSizeFromMetadata, MultipliersForImages, scaleImageDimensions } from '../images'
import * as TP from '../../core/shared/template-path'
import * as PP from '../../core/shared/property-path'
import Canvas, { TargetSearchType } from './canvas'
import {
  CanvasFrameAndTarget,
  CSSCursor,
  DuplicateNewUID,
  EdgePosition,
  flexResizeChange,
  MoveDragState,
  oppositeEdgePositionPart,
  pinFrameChange,
  PinOrFlexFrameChange,
  ResizeDragState,
  singleResizeChange,
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
  Guideline,
  Guidelines,
  GuidelineWithSnappingVector,
  xAxisGuideline,
  yAxisGuideline,
} from './guideline'
import { addImport, mergeImports } from '../../core/workers/common/project-file-utils'
import { getLayoutProperty } from '../../core/layout/getLayoutProperty'
import { getStoryboardUID } from '../../core/model/scene-utils'
import { forceNotNull, optionalMap } from '../../core/shared/optional-utils'
import { fastForEach } from '../../core/shared/utils'
import { UiJsxCanvasContextData } from './ui-jsx-canvas'
import { addFileToProjectContents, contentsToTree, ProjectContentTreeRoot } from '../assets'
import { emptyComments } from '../../core/workers/parser-printer/parser-printer-comments'
import { getAllTargetsAtPoint } from './dom-lookup'
import { WindowMousePositionRaw } from '../../templates/editor-canvas'
import { parseCSSLengthPercent } from '../inspector/common/css-utils'
import { normalisePathToUnderlyingTargetForced } from '../custom-code/code-file'
import { addToMapOfArraysUnique } from '../../core/shared/array-utils'
import { mapValues } from '../../core/shared/object-utils'

export function getOriginalFrames(
  selectedViews: Array<TemplatePath>,
  componentMetadata: ElementInstanceMetadataMap,
): Array<OriginalCanvasAndLocalFrame> {
  let originalFrames: Array<OriginalCanvasAndLocalFrame> = []
  function includeChildren(view: TemplatePath): Array<TemplatePath> {
    return [
      view,
      ...MetadataUtils.getChildrenHandlingGroups(componentMetadata, view, true).map(
        (child) => child.templatePath,
      ),
    ]
  }
  Utils.fastForEach(
    extendSelectedViewsForInteraction(selectedViews, componentMetadata),
    (selectedView) => {
      const allPaths = Utils.flatMapArray(includeChildren, TP.allPaths(selectedView))
      Utils.fastForEach(allPaths, (path) => {
        let alreadyAdded = false
        Utils.fastForEach(originalFrames, (originalFrame) => {
          if (TP.pathsEqual(originalFrame.target, path)) {
            alreadyAdded = true
          }
        })
        if (!alreadyAdded) {
          // TODO Scene Implementation - this should only be one call
          const localFrame = MetadataUtils.getFrame(path, componentMetadata)
          const globalFrame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
          if (localFrame != null && globalFrame != null) {
            // Remove the ancestor frames if the immediate ones are groups.
            let workingFrame: CanvasRectangle | null = canvasRectangle(localFrame)
            workingFrame = MetadataUtils.shiftGroupFrame(
              componentMetadata,
              path,
              workingFrame,
              false,
            )

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
  selectedViews: Array<TemplatePath>,
  componentMetadata: ElementInstanceMetadataMap,
): Array<CanvasFrameAndTarget> {
  const originalFrames: Array<CanvasFrameAndTarget> = []
  function includeChildren(view: TemplatePath): Array<TemplatePath> {
    return [
      view,
      ...MetadataUtils.getChildrenHandlingGroups(componentMetadata, view, true).map(
        (child) => child.templatePath,
      ),
    ]
  }
  Utils.fastForEach(selectedViews, (selectedView) => {
    const selectedAndChildren = Utils.flatMapArray(includeChildren, TP.allPaths(selectedView))
    const includingParents = [...selectedAndChildren, ...selectedAndChildren.map(TP.parentPath)]
    const allPaths = R.uniqBy(TP.toComponentId, Utils.stripNulls(includingParents))
    Utils.fastForEach(allPaths, (path) => {
      let alreadyAdded = false
      Utils.fastForEach(originalFrames, (originalFrame) => {
        if (TP.pathsEqual(originalFrame.target, path)) {
          alreadyAdded = true
        }
      })
      if (!alreadyAdded) {
        const frame = MetadataUtils.getFrameInCanvasCoords(path, componentMetadata)
        originalFrames.push({
          target: path,
          frame: frame,
        })
      }
    })
  })
  return originalFrames
}

function applyTransientFilesState(
  producedTransientFilesState: TransientFilesState | null,
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
  return workingState
}

export function clearDragState(
  model: EditorState,
  derived: DerivedState,
  applyChanges: boolean,
): EditorState {
  let result: EditorState = model
  if (applyChanges && result.canvas.dragState != null && result.canvas.dragState.drag != null) {
    const producedTransientCanvasState = produceCanvasTransientState(
      derived.canvas.transientState.selectedViews,
      result,
      false,
    )
    const producedTransientFilesState = producedTransientCanvasState.filesState
    result = applyTransientFilesState(producedTransientFilesState, result)
  }

  return {
    ...result,
    canvas: {
      ...result.canvas,
      dragState: null,
    },
    selectedViews: applyChanges
      ? derived.canvas.transientState.selectedViews
      : result.selectedViews,
  }
}

export function canvasFrameToNormalisedFrame(frame: CanvasRectangle): NormalisedFrame {
  const { x, y, width, height } = frame
  return { left: x, top: y, width, height }
}

export function updateFramesOfScenesAndComponents(
  editorState: EditorState,
  framesAndTargets: Array<PinOrFlexFrameChange>,
  optionalParentFrame: CanvasRectangle | null,
): EditorState {
  let workingEditorState: EditorState = editorState
  Utils.fastForEach(framesAndTargets, (frameAndTarget) => {
    const target = TP.instancePathForElementAtPath(frameAndTarget.target)

    // Realign to aim at the static version, not the dynamic one.
    const originalTarget = target
    const staticTarget = MetadataUtils.dynamicPathToStaticPath(target)
    if (staticTarget == null) {
      return
    }

    const element = withUnderlyingTargetFromEditorState(
      staticTarget,
      workingEditorState,
      null,
      (success, underlyingElement) => underlyingElement,
    )
    if (element == null) {
      throw new Error(`Unexpected result when looking for element: ${element}`)
    }

    const staticParentPath = TP.parentPath(staticTarget)
    const parentElement = withUnderlyingTargetFromEditorState(
      staticParentPath,
      workingEditorState,
      null,
      (success, underlyingElement) => underlyingElement,
    )

    const isFlexContainer =
      frameAndTarget.type !== 'PIN_FRAME_CHANGE' &&
      frameAndTarget.type !== 'PIN_MOVE_CHANGE' &&
      frameAndTarget.type !== 'PIN_SIZE_CHANGE' &&
      frameAndTarget.type !== 'SINGLE_RESIZE' // TODO since now we are trusting the frameAndTarget.type, there is no point in having two switches

    let propsToSet: Array<ValueAtPath> = []
    let propsToSkip: Array<PropertyPath> = []
    if (isFlexContainer) {
      if (TP.isInstancePath(staticParentPath)) {
        switch (frameAndTarget.type) {
          case 'PIN_FRAME_CHANGE': // this can never run now since frameAndTarget.type cannot be both PIN_FRAME_CHANGE and not PIN_FRAME_CHANGE
          case 'PIN_SIZE_CHANGE': // this can never run now since frameAndTarget.type cannot be both PIN_FRAME_CHANGE and not PIN_FRAME_CHANGE
          case 'PIN_MOVE_CHANGE': // this can never run now since frameAndTarget.type cannot be both PIN_FRAME_CHANGE and not PIN_FRAME_CHANGE
          case 'SINGLE_RESIZE': // this can never run now since frameAndTarget.type cannot be both PIN_FRAME_CHANGE and not PIN_FRAME_CHANGE
            throw new Error(
              `Attempted to make a pin change against an element in a flex container ${JSON.stringify(
                staticParentPath,
              )}.`,
            )
          case 'FLEX_MOVE':
            workingEditorState = modifyUnderlyingForOpenFile(
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
                    workingEditorState.canvas.openFile?.filename ?? null,
                    components,
                    underlyingTarget,
                    frameAndTarget.newIndex,
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
              // Flex based layout.
              const possibleFlexProps = FlexLayoutHelpers.convertWidthHeightToFlex(
                frameAndTarget.newSize.width,
                frameAndTarget.newSize.height,
                element.props,
                right(parentElement.props),
                frameAndTarget.edgePosition,
              )
              forEachRight(possibleFlexProps, (flexProps) => {
                const { flexBasis, width, height } = flexProps
                if (flexBasis != null) {
                  propsToSet.push({
                    path: createLayoutPropertyPath('flexBasis'),
                    value: jsxAttributeValue(flexBasis, emptyComments),
                  })
                }
                if (width != null) {
                  propsToSet.push({
                    path: createLayoutPropertyPath('Width'),
                    value: jsxAttributeValue(width, emptyComments),
                  })
                }
                if (height != null) {
                  propsToSet.push({
                    path: createLayoutPropertyPath('Height'),
                    value: jsxAttributeValue(height, emptyComments),
                  })
                }
              })
            }
            break
          default:
            const _exhaustiveCheck: never = frameAndTarget
            throw new Error(`Unhandled type ${JSON.stringify(frameAndTarget)}`)
        }
      } else {
        // Shouldn't happen, but the types force our hand here.
        throw new Error(
          `Attempted to use non-instance path ${JSON.stringify(
            staticParentPath,
          )} as a flex parent.`,
        )
      }
    } else {
      let parentFrame: CanvasRectangle | null = null
      if (optionalParentFrame == null) {
        const nonGroupParent = MetadataUtils.findNonGroupParent(
          workingEditorState.jsxMetadata,
          originalTarget,
        )
        parentFrame =
          nonGroupParent == null
            ? null
            : MetadataUtils.getFrameInCanvasCoords(nonGroupParent, workingEditorState.jsxMetadata)
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
            const currentLocalFrame = MetadataUtils.getFrame(target, workingEditorState.jsxMetadata)
            const currentFullFrame = optionalMap(Frame.getFullFrame, currentLocalFrame)
            const fullFrame = Frame.getFullFrame(newLocalFrame)
            const elementProps = element.props

            // Pinning layout.
            const frameProps = LayoutPinnedProps.filter((p) => {
              const value = getLayoutProperty(p, right(elementProps))
              return isLeft(value) || value.value != null
            }).map(framePointForPinnedProp)

            function whichPropsToUpdate() {
              if (frameAndTarget.type === 'PIN_SIZE_CHANGE') {
                // only update left, top, right or bottom if the frame is expressed as left, top, right, bottom.
                // otherwise try to change width and height only
                let verticalPoints = frameProps.filter((p) => VerticalFramePoints.includes(p))
                let horizontalPoints = frameProps.filter((p) => HorizontalFramePoints.includes(p))
                if (verticalPoints.length < 2) {
                  verticalPoints.push(FramePoint.Height)
                }
                if (horizontalPoints.length < 2) {
                  horizontalPoints.push(FramePoint.Width)
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
                return frameProps.length == 4
                  ? frameProps
                  : [FramePoint.Left, FramePoint.Top, FramePoint.Width, FramePoint.Height]
              }
            }

            const propsToUpdate: Array<FramePoint> = whichPropsToUpdate()

            Utils.fastForEach(propsToUpdate, (propToUpdate) => {
              const absoluteValue = fullFrame[propToUpdate]
              const previousValue = currentFullFrame == null ? null : currentFullFrame[propToUpdate]

              const pinnedPropToUpdate = pinnedPropForFramePoint(propToUpdate)
              const propPathToUpdate = createLayoutPropertyPath(pinnedPropToUpdate)
              const existingProp = getLayoutProperty(pinnedPropToUpdate, right(elementProps))
              if (absoluteValue === previousValue || isLeft(existingProp)) {
                // Only update pins that have actually changed or aren't set via code
                propsToSkip.push(propPathToUpdate)
              } else {
                const pinIsPercentage =
                  existingProp.value == null ? false : isPercentPin(existingProp.value)
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
                  value: jsxAttributeValue(valueToUse, emptyComments),
                })
              }
            })
          }
          break

        case 'PIN_MOVE_CHANGE': {
          let frameProps: { [k: string]: string | number | undefined } = {} // { FramePoint: value }
          Utils.fastForEach(LayoutPinnedProps, (p) => {
            const framePoint = framePointForPinnedProp(p)
            if (framePoint !== FramePoint.Width && framePoint !== FramePoint.Height) {
              const value = getLayoutProperty(p, right(element.props))
              if (isLeft(value) || value.value != null) {
                frameProps[framePoint] = value.value
                propsToSkip.push(createLayoutPropertyPath(p))
              }
            }
          })

          let framePointsToUse: Array<FramePoint> = [...(Object.keys(frameProps) as FramePoint[])]
          const horizontalExistingFramePoints = framePointsToUse.filter(
            (p) => HorizontalFramePointsExceptSize.indexOf(p) > -1,
          )
          if (horizontalExistingFramePoints.length === 0) {
            framePointsToUse.push(FramePoint.Left)
          }
          const verticalExistingFramePoints = framePointsToUse.filter(
            (p) => VerticalFramePointsExceptSize.indexOf(p) > -1,
          )
          if (verticalExistingFramePoints.length === 0) {
            framePointsToUse.push(FramePoint.Top)
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
          let frameProps: { [k: string]: string | number | undefined } = {} // { FramePoint: value }
          Utils.fastForEach(LayoutPinnedProps, (p) => {
            const framePoint = framePointForPinnedProp(p)
            const value = getLayoutProperty(p, right(element.props))
            if (isLeft(value) || value.value != null) {
              frameProps[framePoint] = value.value
              propsToSkip.push(createLayoutPropertyPath(p))
            }
          })

          let framePointsToUse: Array<FramePoint> = Object.keys(frameProps) as FramePoint[]

          if (isEdgePositionOnSide(frameAndTarget.edgePosition)) {
            framePointsToUse = extendPartialFramePointsForResize(
              framePointsToUse,
              frameAndTarget.edgePosition,
            )
          } else {
            let verticalPoints = framePointsToUse.filter((p) => VerticalFramePoints.includes(p))
            let horizontalPoints = framePointsToUse.filter((p) => HorizontalFramePoints.includes(p))

            if (verticalPoints.length < 2) {
              if (verticalPoints.length === 0) {
                verticalPoints.push(FramePoint.Top)
              }
              verticalPoints.push(FramePoint.Height)
            }
            if (horizontalPoints.length < 2) {
              if (horizontalPoints.length === 0) {
                horizontalPoints.push(FramePoint.Left)
              }
              horizontalPoints.push(FramePoint.Width)
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
        case 'FLEX_MOVE':
        case 'FLEX_RESIZE':
          throw new Error(
            `Attempted to make a flex change against a pinned element ${JSON.stringify(
              staticParentPath,
            )}.`,
          )
        default:
          const _exhaustiveCheck: never = frameAndTarget
          throw new Error(`Unhandled type ${JSON.stringify(frameAndTarget)}`)
      }
    }

    if (propsToSet.length > 0) {
      const propsToNotDelete = [...propsToSet.map((p) => p.path), ...propsToSkip]

      workingEditorState = modifyUnderlyingForOpenFile(
        originalTarget,
        workingEditorState,
        (elem) => {
          // Remove the pinning and flex props first...
          const propsToMaybeRemove =
            frameAndTarget.type === 'PIN_MOVE_CHANGE'
              ? PinningAndFlexPointsExceptSize // for PIN_MOVE_CHANGE, we don't want to remove the size props, we just keep them intact
              : PinningAndFlexPoints
          let propsToRemove: Array<PropertyPath> = []
          function createPropPathForProp(prop: string): PropertyPath {
            if (isFramePoint(prop)) {
              return createLayoutPropertyPath(pinnedPropForFramePoint(prop))
            } else {
              return createLayoutPropertyPath(prop as LayoutProp)
            }
          }
          fastForEach(propsToMaybeRemove, (prop) => {
            const propPath = createPropPathForProp(prop)
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
    workingEditorState = modifyUnderlyingForOpenFile(
      staticTarget,
      workingEditorState,
      roundJSXElementLayoutValues,
    )
    // TODO originalFrames is never being set, so we have a regression here, meaning keepChildrenGlobalCoords
    // doesn't work. Once that is fixed we can re-implement keeping the children in place
  })
  return workingEditorState
}

function updateFrameValueForProp(
  framePoint: FramePoint,
  delta: number,
  frameProps: { [k: string]: string | number | undefined },
  parentFrame: CanvasRectangle | null,
): ValueAtPath | null {
  if (delta !== 0) {
    const existingProp = frameProps[framePoint]
    if (existingProp == null) {
      return {
        path: createLayoutPropertyPath(pinnedPropForFramePoint(framePoint)),
        value: jsxAttributeValue(delta, emptyComments),
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
          const referenceSize = isHorizontalPoint(framePoint)
            ? parentFrame.width
            : parentFrame.height
          const deltaAsPercentValue = (delta / referenceSize) * 100
          valueToUse = `${percentValue + deltaAsPercentValue}%`
        } else {
          valueToUse = `${percentValue + delta}%`
        }
        return {
          path: createLayoutPropertyPath(pinnedPropForFramePoint(framePoint)),
          value: jsxAttributeValue(valueToUse, emptyComments),
        }
      } else if (pinIsUnitlessOrPx) {
        return {
          path: createLayoutPropertyPath(pinnedPropForFramePoint(framePoint)),
          value: jsxAttributeValue(parsedProp.value + delta, emptyComments),
        }
      }
    }
  }
  return null
}

function getPropsToSetToMoveElement(
  dragDelta: CanvasVector,
  framePoints: FramePoint[],
  frameProps: { [k: string]: string | number | undefined },
  parentFrame: CanvasRectangle | null,
): ValueAtPath[] {
  let propsToSet: ValueAtPath[] = []
  Utils.fastForEach(framePoints, (framePoint) => {
    const delta = isHorizontalPoint(framePoint) ? dragDelta.x : dragDelta.y
    const shouldInvertValue = framePoint === FramePoint.Right || framePoint === FramePoint.Bottom
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
  framePoints: FramePoint[],
  frameProps: { [k: string]: string | number | undefined },
  parentFrame: CanvasRectangle | null,
): ValueAtPath[] {
  let propsToSet: ValueAtPath[] = []
  Utils.fastForEach(framePoints, (framePoint) => {
    let updatedProp
    switch (framePoint) {
      case FramePoint.Left: {
        const targetEdgePoint = { x: 0, y: 0.5 }
        const delta = widthDelta * (edgePosition.x + targetEdgePoint.x - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case FramePoint.Top: {
        const targetEdgePoint = { x: 0.5, y: 0 }
        const delta = heightDelta * (edgePosition.y + targetEdgePoint.y - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case FramePoint.Right: {
        const targetEdgePoint = { x: 1, y: 0.5 }
        const delta = widthDelta * -(edgePosition.x + targetEdgePoint.x - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case FramePoint.Bottom: {
        const targetEdgePoint = { x: 0.5, y: 1 }
        const delta = heightDelta * -(edgePosition.y + targetEdgePoint.y - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case FramePoint.Width: {
        if (widthDelta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, widthDelta, frameProps, parentFrame)
        }
        break
      }
      case FramePoint.Height: {
        if (heightDelta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, heightDelta, frameProps, parentFrame)
        }
        break
      }
      case FramePoint.CenterX: {
        const targetEdgePoint = { x: 0.5, y: 0.5 }
        const delta = widthDelta * (edgePosition.x + targetEdgePoint.x - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
        }
        break
      }
      case FramePoint.CenterY: {
        const targetEdgePoint = { x: 0.5, y: 0.5 }
        const delta = heightDelta * (edgePosition.y + targetEdgePoint.y - 1)
        if (delta !== 0) {
          updatedProp = updateFrameValueForProp(framePoint, delta, frameProps, parentFrame)
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

function extendPartialFramePointsForResize(frameProps: FramePoint[], edgePosition: EdgePosition) {
  // if it has partial positioning points set and dragged on an edge only the dragged edge should be added while keeping the existing frame points.
  let verticalPoints = frameProps.filter((p) => VerticalFramePoints.includes(p))
  let horizontalPoints = frameProps.filter((p) => HorizontalFramePoints.includes(p))
  let framePointsToUse = [...frameProps]
  if (edgePosition.x === 0.5 && verticalPoints.length < 2) {
    if (verticalPoints.length === 0) {
      if (edgePosition.y === 0) {
        verticalPoints.push(FramePoint.Top)
        verticalPoints.push(FramePoint.Height)
      } else {
        verticalPoints.push(FramePoint.Height)
      }
    } else {
      if (edgePosition.y === 0) {
        verticalPoints.push(FramePoint.Top)
      } else if (!verticalPoints.includes(FramePoint.Bottom)) {
        verticalPoints.push(FramePoint.Height)
      }
    }
    framePointsToUse = [...verticalPoints, ...horizontalPoints]
  }
  if (edgePosition.y === 0.5 && horizontalPoints.length < 2) {
    if (horizontalPoints.length === 0) {
      if (edgePosition.x === 0) {
        horizontalPoints.push(FramePoint.Left)
        horizontalPoints.push(FramePoint.Width)
      } else {
        horizontalPoints.push(FramePoint.Width)
      }
    } else {
      if (edgePosition.x === 0) {
        horizontalPoints.push(FramePoint.Left)
      } else if (!horizontalPoints.includes(FramePoint.Right)) {
        horizontalPoints.push(FramePoint.Width)
      }
    }
    framePointsToUse = [...verticalPoints, ...horizontalPoints]
  }
  return Utils.uniq(framePointsToUse)
}

export function getOriginalFrameInCanvasCoords(
  originalFrames: Array<OriginalCanvasAndLocalFrame>,
  target: TemplatePath,
): CanvasRectangle | null {
  for (const originalFrame of originalFrames || []) {
    if (TP.pathsEqual(target, originalFrame.target)) {
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
  selectedViews: Array<TemplatePath>,
  scale: number,
  draggedPoint: CanvasPoint | null,
  resizingFromPosition: EdgePosition | null,
): Array<GuidelineWithSnappingVector> {
  if (draggedPoint == null) {
    return []
  }

  let guidelines: Array<Guideline> = collectParentAndSiblingGuidelines(metadata, selectedViews)
  // For any images create guidelines at the current multiplier setting.
  if (resizingFromPosition != null) {
    Utils.fastForEach(selectedViews, (selectedView) => {
      if (TP.isScenePath(selectedView)) {
        return
      }

      if (MetadataUtils.isPinnedAndNotAbsolutePositioned(metadata, selectedView)) {
        return
      }

      const instance = MetadataUtils.getElementByInstancePathMaybe(metadata, selectedView)
      if (instance != null && MetadataUtils.isImg(instance) && instance.localFrame != null) {
        const frame = instance.localFrame
        const imageSize = getImageSizeFromMetadata(instance)
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
                guidelines.push(
                  cornerGuideline(
                    point.x + imageDimension.width,
                    point.y + imageDimension.height,
                    -imageDimension.width,
                    -imageDimension.height,
                  ),
                )
              } else {
                // Bottom-left.
                guidelines.push(
                  cornerGuideline(
                    point.x,
                    point.y + imageDimension.height,
                    imageDimension.width,
                    -imageDimension.height,
                  ),
                )
              }
            } else {
              if (resizingFromPosition.y === 0) {
                // Top-right.
                guidelines.push(
                  cornerGuideline(
                    point.x + imageDimension.width,
                    point.y,
                    -imageDimension.width,
                    imageDimension.height,
                  ),
                )
              } else {
                // Bottom-right.
                guidelines.push(
                  cornerGuideline(point.x, point.y, imageDimension.width, imageDimension.height),
                )
              }
            }
          } else if (isEdgePositionAVerticalEdge(resizingFromPosition)) {
            // If this is a side edge the guidelines will be at x +/- width and y +/- (height / 2).
            guidelines.push(
              xAxisGuideline(
                point.x - imageDimension.width,
                point.y - lowHalfHeight,
                point.y + highHalfHeight,
              ),
              xAxisGuideline(
                point.x + imageDimension.width,
                point.y - lowHalfHeight,
                point.y + highHalfHeight,
              ),
              yAxisGuideline(point.y - lowHalfHeight, point.x - imageDimension.width, point.x),
              yAxisGuideline(point.y - lowHalfHeight, point.x, point.x + imageDimension.width),
              yAxisGuideline(point.y + highHalfHeight, point.x - imageDimension.width, point.x),
              yAxisGuideline(point.y + highHalfHeight, point.x, point.x + imageDimension.width),
            )
          } else if (isEdgePositionAHorizontalEdge(resizingFromPosition)) {
            // If this is a top/bottom edge the guidelines will be at x +/- (width / 2) and y +/- height.
            guidelines.push(
              xAxisGuideline(point.x - lowHalfWidth, point.y - imageDimension.height, point.y),
              xAxisGuideline(point.x - lowHalfWidth, point.y, point.y + imageDimension.height),
              xAxisGuideline(point.x + highHalfWidth, point.y - imageDimension.height, point.y),
              xAxisGuideline(point.x + highHalfWidth, point.y, point.y + imageDimension.height),
              yAxisGuideline(
                point.y - imageDimension.height,
                point.x - lowHalfWidth,
                point.x + highHalfWidth,
              ),
              yAxisGuideline(
                point.y + imageDimension.height,
                point.x - lowHalfWidth,
                point.x + highHalfWidth,
              ),
            )
          }
        })
      }
    })
  }
  const filteredGuidelines =
    resizingFromPosition != null
      ? filterGuidelinesStaticAxis(guidelines, resizingFromPosition)
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
  editor: EditorState,
  point: CanvasPoint,
  resizingFromPosition: EdgePosition | null,
): { point: CanvasPoint; guideline: GuidelineWithSnappingVector | null } {
  const guidelines = oneGuidelinePerDimension(
    collectGuidelines(
      editor.jsxMetadata,
      editor.selectedViews,
      editor.canvas.scale,
      point,
      resizingFromPosition,
    ),
  )
  let snappedPoint = point
  let snappedGuideline: GuidelineWithSnappingVector | null = null

  guidelines.forEach((guideline) => {
    if (guideline.activateSnap) {
      snappedPoint = Utils.offsetPoint(snappedPoint, guideline.snappingVector)
      snappedGuideline = guideline
    }
  })
  return {
    point: snappedPoint,
    guideline: snappedGuideline,
  }
}

export function snapPoint(
  editor: EditorState,
  pointToSnap: CanvasPoint,
  enableSnapping: boolean,
  keepAspectRatio: boolean,
  diagonalA: CanvasPoint,
  diagonalB: CanvasPoint,
  resizingFromPosition: EdgePosition | null,
): CanvasPoint {
  const elementsToTarget = determineElementsToOperateOnForDragging(
    editor.selectedViews,
    editor.jsxMetadata,
    true,
    false,
  )
  const anythingPinnedAndNotAbsolutePositioned = elementsToTarget.some((elementToTarget) => {
    return MetadataUtils.isPinnedAndNotAbsolutePositioned(editor.jsxMetadata, elementToTarget)
  })
  const shouldSnap = enableSnapping && !anythingPinnedAndNotAbsolutePositioned

  if (keepAspectRatio) {
    const closestPointOnLine = Utils.closestPointOnLine(diagonalA, diagonalB, pointToSnap)
    if (shouldSnap) {
      const { guideline } = innerSnapPoint(editor, closestPointOnLine, resizingFromPosition)
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
          return snappedPoint
        }
      }
      // fallback to regular diagonal snapping
      return closestPointOnLine
    } else {
      return pointToSnap
    }
  } else {
    const { point } = innerSnapPoint(editor, pointToSnap, resizingFromPosition)
    return shouldSnap ? point : pointToSnap
  }
}

function calculateDraggedRectangle(
  editor: EditorState,
  dragState: ResizeDragState,
): CanvasRectangle {
  const originalSize = dragState.originalSize
  const deltaScale = dragState.centerBasedResize ? 2 : 1 // for center based resize, we need to calculate with double deltas
  // for now, because scale is not a first-class citizen, we know that CanvasVector and LocalVector have the same dimensions
  // this will break with the introduction of scale into the coordinate systems
  let delta: CanvasVector = canvasPoint({ x: 0, y: 0 })
  if (dragState.drag != null) {
    delta = Utils.scaleVector(
      Utils.scalePoint(dragState.drag, dragState.enabledDirection as CanvasVector),
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
      editor,
      newCorner,
      dragState.enableSnapping,
      dragState.keepAspectRatio,
      startingPoint,
      draggedCorner,
      startingCorner,
    ),
    0,
  )
  const newSizeVector = Utils.pointDifference(startingPoint, snappedNewCorner)
  const newRectangle = dragState.centerBasedResize
    ? Utils.rectFromPointVector(originalCenter, Utils.scaleVector(newSizeVector, 0.5), true)
    : Utils.rectFromPointVector(startingPoint, newSizeVector, false)
  return newRectangle
}

export function calculateNewBounds(
  editor: EditorState,
  dragState: ResizeDragState,
): CanvasRectangle {
  const originalSize = dragState.originalSize
  const aspectRatio = originalSize.width / originalSize.height
  const newRectangle = calculateDraggedRectangle(editor, dragState)

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
    const newWidth = dragState.keepAspectRatio
      ? Utils.roundTo(newRectangle.height * aspectRatio)
      : originalSize.width
    newRectangle.x -= newWidth / 2
    newRectangle.width = newWidth
  }
  if (dragState.edgePosition.y === 0.5) {
    const newHeight = dragState.keepAspectRatio
      ? Utils.roundTo(newRectangle.width / aspectRatio)
      : originalSize.height
    newRectangle.y -= newHeight / 2
    newRectangle.height = newHeight
  }

  return newRectangle
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
  elementsToTarget: Array<TemplatePath>,
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
    mapValues((success) => {
      return transientFileState(success.topLevelElements, success.imports)
    }, successByFilename),
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
    return transientCanvasState(dragState.draggedElements, editorState.highlightedViews, null)
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
            const isFlexContainer = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
              target,
              editorState.jsxMetadata,
            )

            let change: PinOrFlexFrameChange
            if (isFlexContainer) {
              framesAndTargets.push(
                flexResizeChange(underlyingTarget, roundedFrame, dragState.edgePosition),
              )
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
    return transientCanvasState(editorState.selectedViews, editorState.highlightedViews, null)
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
        const isFlexContainer = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
          elementToTarget,
          editorState.jsxMetadata,
        )
        if (isFlexContainer) {
          framesAndTargets.push(
            flexResizeChange(elementToTarget, roundedFrame, dragState.edgePosition),
          )
        } else {
          const edgePosition = dragState.centerBasedResize
            ? ({ x: 0.5, y: 0.5 } as EdgePosition)
            : dragState.edgePosition
          const sizeChange = {
            x: roundedFrame.width - originalFrame.width,
            y: roundedFrame.height - originalFrame.height,
          } as CanvasVector
          framesAndTargets.push(singleResizeChange(elementToTarget, edgePosition, sizeChange))
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
  previousCanvasTransientSelectedViews: Array<TemplatePath>,
  editorState: EditorState,
  preventAnimations: boolean,
): TransientCanvasState {
  const currentOpenFile = editorState.canvas.openFile?.filename
  let transientState: TransientCanvasState | null = null
  if (currentOpenFile != null) {
    const editorMode = editorState.mode
    switch (editorMode.type) {
      case 'insert':
        if (insertionSubjectIsJSXElement(editorMode.subject) && editorMode.insertionStarted) {
          const insertionElement = editorMode.subject.element
          const importsToAdd = editorMode.subject.importsToAdd
          const insertionParent = editorMode.subject.parent?.target ?? null
          const insertionParentAsInstancePath = TP.isScenePath(insertionParent)
            ? TP.instancePathForElementAtScenePath(insertionParent)
            : insertionParent

          // Not actually modifying the underlying target, but we'll exploit the functionality.
          modifyUnderlyingTarget(
            insertionParentAsInstancePath,
            currentOpenFile,
            editorState,
            (element) => element,
            (parseSuccess, underlying, underlyingFilePath) => {
              const openComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)

              const updatedComponents = insertJSXElementChild(
                editorState.projectContents,
                currentOpenFile,
                underlying,
                insertionElement,
                openComponents,
                {
                  type: 'front',
                },
              )
              const updatedImports: Imports = mergeImports(parseSuccess.imports, importsToAdd)

              // Sync these back up.
              const topLevelElements = applyUtopiaJSXComponentsChanges(
                parseSuccess.topLevelElements,
                updatedComponents,
              )

              transientState = transientCanvasState(
                editorState.selectedViews,
                editorState.highlightedViews,
                {
                  [underlyingFilePath]: transientFileState(topLevelElements, updatedImports),
                },
              )
              return parseSuccess
            },
          )
        }
        break
      case 'select':
        if (
          editorState.canvas.dragState != null &&
          editorState.canvas.dragState.start != null &&
          editorState.canvas.dragState.drag != null
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
  }

  if (transientState == null) {
    return transientCanvasState(editorState.selectedViews, editorState.highlightedViews, null)
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
  selectedViews: Array<TemplatePath>,
  componentMetadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
): Array<DuplicateNewUID> {
  const targetViews = determineElementsToOperateOnForDragging(
    selectedViews,
    componentMetadata,
    true,
    false,
  )

  let existingIDs = getAllUniqueUids(projectContents)

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

export function filterMultiSelectScenes(targets: Array<TemplatePath>): Array<TemplatePath> {
  // TODO Scene Implementation - KILLME
  const scenesSelected = targets.some(TP.isScenePath)
  if (scenesSelected && targets.length > 1) {
    // Prevent multiselection of scenes - if more than one are selected, only take the last one
    return [R.findLast(TP.isScenePath, targets)!]
  } else {
    return targets
  }
}

function getReparentTargetAtPosition(
  componentMeta: ElementInstanceMetadataMap,
  selectedViews: Array<TemplatePath>,
  hiddenInstances: Array<TemplatePath>,
  canvasScale: number,
  canvasOffset: CanvasVector,
): TemplatePath | undefined {
  const allTargets = getAllTargetsAtPoint(
    componentMeta,
    selectedViews,
    hiddenInstances,
    'no-filter',
    WindowMousePositionRaw,
    canvasScale,
    canvasOffset,
  )
  // filtering for non-selected views from alltargets
  return R.head(
    allTargets.filter((target) => selectedViews.every((view) => !TP.pathsEqual(view, target))),
  )
}

export function getReparentTarget(
  selectedViews: Array<TemplatePath>,
  editorState: EditorState,
  toReparent: Array<TemplatePath>,
  position: CanvasPoint,
): {
  shouldReparent: boolean
  newParent: TemplatePath | null
} {
  const result = getReparentTargetAtPosition(
    editorState.jsxMetadata,
    selectedViews,
    editorState.hiddenInstances,
    editorState.canvas.scale,
    editorState.canvas.realCanvasOffset,
  )
  const possibleNewParent = result == undefined ? null : result
  const currentParents = Utils.stripNulls(
    toReparent.map((view) => MetadataUtils.getParent(editorState.jsxMetadata, view)),
  )
  let parentSupportsChild = true
  if (possibleNewParent != null) {
    parentSupportsChild = withUnderlyingTarget(
      possibleNewParent,
      editorState.projectContents,
      editorState.nodeModules.files,
      editorState.canvas.openFile?.filename,
      false,
      (success) => {
        return MetadataUtils.targetSupportsChildren(
          success.imports,
          editorState.jsxMetadata,
          possibleNewParent,
        )
      },
    )
  } else {
    // a null template path means Canvas, let's translate that to the storyboard component
    const storyboardComponent = getStoryboardTemplatePathFromEditorState(editorState)
    return {
      shouldReparent: storyboardComponent != null,
      newParent: storyboardComponent,
    }
  }
  if (
    parentSupportsChild &&
    ((currentParents.length === 0 && possibleNewParent != null) ||
      (currentParents.length > 0 &&
        currentParents.every((parent) => !TP.pathsEqual(possibleNewParent, parent.templatePath))))
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
  newPath: TemplatePath | null
}

export function getFrameChange(
  target: TemplatePath,
  newFrame: CanvasRectangle,
  isParentFlex: boolean,
): PinOrFlexFrameChange {
  if (isParentFlex) {
    return flexResizeChange(target, newFrame, null)
  } else {
    return pinFrameChange(target, newFrame, null)
  }
}

export function moveTemplate(
  targetPath: TemplatePath,
  originalPath: TemplatePath,
  newFrame: CanvasRectangle | typeof SkipFrameChange | null,
  indexPosition: IndexPosition,
  newParentPath: TemplatePath | null,
  parentFrame: CanvasRectangle | null,
  editorState: EditorState,
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: Array<TemplatePath>,
  highlightedViews: Array<TemplatePath>,
  newParentLayoutSystem: LayoutSystem | null,
): MoveTemplateResult {
  function noChanges(): MoveTemplateResult {
    return {
      updatedEditorState: editorState,
      newPath: target,
    }
  }
  const target = TP.instancePathForElementAtPath(targetPath)
  let newIndex: number = 0
  let newInstancePath: InstancePath | null = null
  let flexContextChanged: boolean = false

  const targetID = TP.toTemplateId(target)
  if (newParentPath == null || TP.isScenePath(newParentPath) || TP.isScenePath(originalPath)) {
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
            const utopiaComponentsIncludingScenes = getUtopiaJSXComponentsFromSuccess(
              newParentSuccess,
            )
            const {
              components: withLayoutUpdatedForNewContext,
              componentMetadata: withMetadataUpdatedForNewContext,
              didSwitch,
            } = maybeSwitchLayoutProps(
              target,
              originalPath,
              newParentPath,
              componentMetadata,
              componentMetadata,
              utopiaComponentsIncludingScenes,
              parentFrame,
              newParentLayoutSystem,
            )
            const updatedUnderlyingElement = findElementAtPath(
              underlyingTarget,
              withLayoutUpdatedForNewContext,
            )
            if (updatedUnderlyingElement == null) {
              return noChanges()
            } else {
              let workingEditorState: EditorState = editorState

              let updatedUtopiaComponents: Array<UtopiaJSXComponent> = withLayoutUpdatedForNewContext
              let newPath: TemplatePath | null = null

              flexContextChanged = flexContextChanged || didSwitch

              // Remove and then insert again at the new location.
              workingEditorState = modifyParseSuccessAtPath(
                underlyingNewParentFilePath,
                workingEditorState,
                (workingSuccess) => {
                  updatedUtopiaComponents = removeElementAtPath(
                    underlyingTarget,
                    updatedUtopiaComponents,
                  )

                  updatedUtopiaComponents = insertElementAtPath(
                    workingEditorState.projectContents,
                    workingEditorState.canvas.openFile?.filename ?? null,
                    underlyingNewParentPath,
                    updatedUnderlyingElement,
                    updatedUtopiaComponents,
                    indexPosition,
                  )

                  return {
                    ...workingSuccess,
                    topLevelElements: applyUtopiaJSXComponentsChanges(
                      workingSuccess.topLevelElements,
                      updatedUtopiaComponents,
                    ),
                  }
                },
              )

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
                newIndex = updatedUnderlyingNewParentElement.children.indexOf(
                  updatedUnderlyingElement,
                )
                if (newIndex === -1) {
                  throw new Error('Invalid child element index.')
                }
              }

              newPath = TP.appendToPath(newParentPath, targetID)
              newInstancePath = newPath

              let updatedComponentMetadata: ElementInstanceMetadataMap = withMetadataUpdatedForNewContext
              // Need to make these changes ahead of updating the frame.
              const elementMetadata = MetadataUtils.getElementByInstancePathMaybe(
                updatedComponentMetadata,
                target,
              )

              if (elementMetadata != null) {
                updatedComponentMetadata = MetadataUtils.removeElementMetadataChild(
                  target,
                  updatedComponentMetadata,
                )

                updatedComponentMetadata = MetadataUtils.insertElementMetadataChild(
                  newParentPath,
                  elementMetadata,
                  updatedComponentMetadata,
                  indexPosition,
                )

                updatedComponentMetadata = MetadataUtils.transformAllPathsInMetadata(
                  updatedComponentMetadata,
                  target,
                  newInstancePath,
                )
              }
              workingEditorState.jsxMetadata = updatedComponentMetadata

              if (
                newFrame !== SkipFrameChange &&
                newFrame != null &&
                newInstancePath != null &&
                !flexContextChanged
              ) {
                const isParentFlex = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
                  originalPath,
                  componentMetadata,
                )
                const frameChanges: Array<PinOrFlexFrameChange> = [
                  getFrameChange(newInstancePath, newFrame, isParentFlex),
                ]

                workingEditorState = updateFramesOfScenesAndComponents(
                  workingEditorState,
                  frameChanges,
                  parentFrame,
                )
              }

              const newSelectedViews = selectedViews.map((v) => {
                if (TP.pathsEqual(v, target)) {
                  return newInstancePath
                } else {
                  return v
                }
              })

              const newHighlightedViews =
                newParentPath == null
                  ? highlightedViews.map((t) => (TP.pathsEqual(t, target) ? newInstancePath : t))
                  : [newParentPath]

              const updatedEditorState: EditorState = {
                ...workingEditorState,
                selectedViews: filterMultiSelectScenes(Utils.stripNulls(newSelectedViews)),
                highlightedViews: Utils.stripNulls(newHighlightedViews),
              }

              return {
                updatedEditorState: updatedEditorState,
                newPath: newInstancePath,
              }
            }
          },
        )
      },
    )
  }
}

function preventAnimationsOnTargets(
  editorState: EditorState,
  targets: TemplatePath[],
): EditorState {
  let workingEditorState = editorState
  Utils.fastForEach(targets, (target) => {
    const staticPath = TP.dynamicPathToStaticPath(target)
    if (staticPath != null) {
      workingEditorState = modifyUnderlyingForOpenFile(
        staticPath,
        editorState,
        (underlyingElement) => {
          const styleUpdated = setJSXValuesAtPaths(underlyingElement.props, [
            {
              path: PP.create(['style', 'transition']),
              value: jsxAttributeValue('none', emptyComments),
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
  previousCanvasTransientSelectedViews: Array<TemplatePath>,
  editorState: EditorState,
  dragState: MoveDragState,
  preventAnimations: boolean,
): TransientCanvasState {
  let selectedViews: Array<TemplatePath> = dragState.draggedElements
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
    const reparentTarget = getReparentTarget(
      previousCanvasTransientSelectedViews,
      workingEditorState,
      elementsToTarget,
      dragState.canvasPosition,
    )

    if (reparentTarget.shouldReparent) {
      elementsToTarget = elementsToTarget.map((target) => {
        const frame = originalFrames.find((originalFrameAndTarget) => {
          return TP.pathsEqual(originalFrameAndTarget.target, target)
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
        )
        selectedViews = reparentResult.updatedEditorState.selectedViews
        // As it has moved, we need to synchronise the paths.
        originalFrames = originalFrames.map((originalFrame) => {
          if (reparentResult.newPath != null && TP.pathsEqual(originalFrame.target, target)) {
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
    selectedViews,
  )
  const framesAndTargets = dragComponent(
    workingEditorState.jsxMetadata,
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
    transientFilesState,
  )
}

export function getCanvasOffset(
  previousOffset: CanvasPoint,
  previousScale: number,
  scale: number,
  componentMetadata: ElementInstanceMetadataMap,
  selectedViews: TemplatePath[],
  focusPoint: CanvasPoint | null,
  isFirstLoad: boolean,
): CanvasPoint {
  // TODO HACK getting the canvas element's size here, this should probably go away once we manage the size of the panes
  const canvasDiv = document.getElementById('canvas-root')
  const pinFocusPointOnScreen = focusPoint != null
  const canvasDivSize = canvasDiv == null ? null : canvasDiv.getBoundingClientRect()
  if (canvasDivSize != null && canvasDivSize.width !== 0 && canvasDivSize.height !== 0) {
    const zoomFocusPoint =
      focusPoint ||
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

        if (frame == null) {
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
  selectedViews: Array<TemplatePath>,
  scale: number,
  previousScale: number,
  componentMetadata: ElementInstanceMetadataMap,
  canvasOffset: CanvasPoint,
  canvasDivSize: ClientRect,
): CanvasPoint {
  if (selectedViews.length > 0) {
    const accumulatedPoint = selectedViews.reduce((working, selectedView) => {
      const frame = MetadataUtils.getFrameInCanvasCoords(selectedView, componentMetadata)

      if (frame == null) {
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
  paths: Array<TemplatePath>,
  newParentPath: TemplatePath | null,
  editor: EditorState,
): DuplicateResult | null {
  let duplicateNewUIDs: Array<DuplicateNewUID> = []
  let newOriginalFrames: Array<CanvasFrameAndTarget> | null = null
  if (
    editor.canvas.dragState != null &&
    editor.canvas.dragState.type === 'MOVE_DRAG_STATE' &&
    editor.canvas.dragState.duplicateNewUIDs != null
  ) {
    duplicateNewUIDs = editor.canvas.dragState.duplicateNewUIDs
    newOriginalFrames = editor.canvas.dragState.originalFrames
  }

  let newSelectedViews: Array<TemplatePath> = []
  let workingEditorState: EditorState = editor
  const existingIDs = getAllUniqueUids(editor.projectContents)
  for (const path of paths) {
    let metadataUpdate: (metadata: ElementInstanceMetadataMap) => ElementInstanceMetadataMap = (
      metadata,
    ) => metadata
    workingEditorState = modifyUnderlyingForOpenFile(
      path,
      workingEditorState,
      (elem) => elem,
      (success, underlyingInstancePath, underlyingFilePath) => {
        let utopiaComponents = getUtopiaJSXComponentsFromSuccess(success)
        let newElement: JSXElementChild | null = null
        let jsxElement: JSXElementChild | null = null
        if (TP.isScenePath(path)) {
          const scenepath = TP.instancePathForElementAtScenePath(path)
          jsxElement = findJSXElementChildAtPath(utopiaComponents, scenepath)
        } else {
          jsxElement = findElementAtPath(underlyingInstancePath, utopiaComponents)
        }
        let uid: string
        if (jsxElement == null) {
          console.warn(`Could not find element ${TP.toVarSafeComponentId(path)}`)
          return success
        } else {
          const duplicateNewUID: DuplicateNewUID | undefined = duplicateNewUIDs.find((entry) =>
            TP.pathsEqual(entry.originalPath, path),
          )
          if (duplicateNewUID === undefined) {
            newElement = guaranteeUniqueUids([jsxElement], existingIDs)[0]
            uid = getUtopiaID(newElement)
          } else {
            // Helps to keep the model consistent because otherwise the dom walker
            // goes into a frenzy.
            newElement = setUtopiaID(jsxElement, duplicateNewUID.newUID)
            if (isJSXElement(newElement)) {
              newElement = {
                ...newElement,
                children: guaranteeUniqueUids(newElement.children, [
                  ...existingIDs,
                  duplicateNewUID.newUID,
                ]),
              }
            }
            uid = duplicateNewUID.newUID
          }
          let newPath: TemplatePath
          if (newParentPath == null) {
            const storyboardUID = Utils.forceNotNull(
              'Could not find storyboard element',
              getStoryboardUID(utopiaComponents),
            )
            newPath = TP.scenePath([[storyboardUID, uid]])
          } else {
            newPath = TP.appendToPath(newParentPath, uid)
          }
          // Update the original frames to be the duplicate ones.
          if (newOriginalFrames != null && newPath != null) {
            newOriginalFrames = newOriginalFrames.map((originalFrame) => {
              if (TP.pathsEqual(originalFrame.target, path)) {
                return {
                  frame: originalFrame.frame,
                  target: newPath as TemplatePath,
                }
              } else {
                return originalFrame
              }
            })
          }
          if (TP.isScenePath(path) && isJSXElement(jsxElement)) {
            const numberOfScenes = getNumberOfScenes(editor)
            const newSceneLabel = `Scene ${numberOfScenes}`
            let props = setJSXAttributesAttribute(
              jsxElement.props,
              'data-label',
              jsxAttributeValue(newSceneLabel, emptyComments),
            )
            props = setJSXAttributesAttribute(
              props,
              'data-uid',
              jsxAttributeValue(getUtopiaID(newElement), emptyComments),
            )

            const newSceneElement = {
              ...jsxElement,
              props: props,
            }
            utopiaComponents = addSceneToJSXComponents(
              workingEditorState.projectContents,
              workingEditorState.canvas.openFile?.filename ?? null,
              utopiaComponents,
              newSceneElement,
            )
          } else {
            utopiaComponents = insertElementAtPath(
              workingEditorState.projectContents,
              workingEditorState.canvas.openFile?.filename ?? null,
              newParentPath,
              newElement,
              utopiaComponents,
              null,
            )
          }

          if (newElement == null) {
            console.warn(`Could not duplicate ${TP.toVarSafeComponentId(path)}`)
            return success
          } else {
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
              topLevelElements: applyUtopiaJSXComponentsChanges(
                success.topLevelElements,
                utopiaComponents,
              ),
            }
          }
        }
      },
    )
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
  openFile: string | null,
  components: Array<UtopiaJSXComponent>,
  target: InstancePath,
  newIndex: number,
): Array<UtopiaJSXComponent> {
  let workingComponents = [...components]

  const parentPath = TP.parentPath(target)
  const jsxElement = findElementAtPath(target, workingComponents)

  if (jsxElement != null) {
    const newPosition: IndexPosition = {
      type: 'absolute',
      index: newIndex,
    }

    workingComponents = removeElementAtPath(target, workingComponents)

    workingComponents = insertElementAtPath(
      projectContents,
      openFile,
      parentPath,
      jsxElement,
      workingComponents,
      newPosition,
    )
  }

  return workingComponents
}

export function createTestProjectWithCode(appUiJsFile: string): PersistentModel {
  const baseModel = defaultProject()
  const parsedFile = lintAndParse(StoryboardFilePath, appUiJsFile, null) as ParsedTextFile

  if (isParseFailure(parsedFile)) {
    fail('The test file parse failed')
  }

  return {
    ...baseModel,
    projectContents: addFileToProjectContents(
      baseModel.projectContents,
      StoryboardFilePath,
      textFile(
        textFileContents(appUiJsFile, parsedFile, RevisionsState.BothMatch),
        null,
        Date.now(),
      ),
    ),
  }
}

export function cullSpyCollector(
  spyCollector: UiJsxCanvasContextData,
  domMetadata: ReadonlyArray<ElementInstanceMetadata>,
): void {
  // Note: Mutates `spyCollector`.
  // Collate all the valid paths.
  let elementPaths: Set<string> = Utils.emptySet()
  let scenePaths: Set<string> = Utils.emptySet()
  fastForEach(domMetadata, (element) => {
    let workingPath: TemplatePath | null = element.templatePath
    while (workingPath != null && !TP.isEmptyPath(workingPath)) {
      const pathAsString = TP.toString(workingPath)
      if (TP.isScenePath(workingPath)) {
        elementPaths.add(TP.toString(TP.instancePathForElementAtScenePath(workingPath)))
        scenePaths.add(pathAsString)
      } else {
        elementPaths.add(pathAsString)
      }
      workingPath = TP.parentPath(workingPath)
    }
  })
  // Eliminate the element paths which are invalid.
  fastForEach(Object.keys(spyCollector.current.spyValues.metadata), (elementPath) => {
    if (!elementPaths.has(elementPath)) {
      delete spyCollector.current.spyValues.metadata[elementPath]
    }
  })
}
