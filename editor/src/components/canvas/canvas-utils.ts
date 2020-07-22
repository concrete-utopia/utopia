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
  ComponentMetadata,
  isJSXElement,
  jsxAttributeValue,
  JSXElement,
  JSXElementChild,
  UtopiaJSXComponent,
  ElementInstanceMetadata,
} from '../../core/shared/element-template'
import {
  getAllUniqueUids,
  getUtopiaID,
  guaranteeUniqueUids,
  insertJSXElementChild,
  setUtopiaID,
  transformJSXComponentAtPath,
  findJSXElementChildAtPath,
} from '../../core/model/element-template-utils'
import { generateUID } from '../../core/shared/uid-utils'
import {
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  ValueAtPath,
  setJSXValueAtPath,
  jsxAttributesToProps,
  jsxSimpleAttributeToValue,
} from '../../core/shared/jsx-attributes'
import {
  CanvasMetadata,
  Imports,
  InstancePath,
  isParseFailure,
  ParseResult,
  ParseSuccess,
  RevisionsState,
  TemplatePath,
  importAlias,
  PropertyPath,
} from '../../core/shared/project-file-types'
import {
  getOrDefaultScenes,
  getUtopiaJSXComponentsFromSuccess,
  uiJsFile,
  updateCanvasMetadataParseResult,
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
  applyUtopiaJSXComponentsChanges,
  DerivedState,
  EditorState,
  getOpenImportsFromState,
  getOpenUIJSFile,
  getOpenUtopiaJSXComponentsFromState,
  insertElementAtPath,
  modifyOpenParseSuccess,
  openFileTab,
  OriginalCanvasAndLocalFrame,
  PersistentModel,
  removeElementAtPath,
  TransientCanvasState,
  transientCanvasState,
  transientFileState,
  getStoryboardTemplatePathFromEditorState,
  addSceneToJSXComponents,
  getNumberOfScenes,
  getStoryboardUID,
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
import { createSceneTemplatePath, PathForSceneStyle } from '../../core/model/scene-utils'
import { optionalMap } from '../../core/shared/optional-utils'
import { fastForEach } from '../../core/shared/utils'
import { UiJsxCanvasContextData } from './ui-jsx-canvas'

export function getOriginalFrames(
  selectedViews: Array<TemplatePath>,
  componentMetadata: Array<ComponentMetadata>,
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
  componentMetadata: Array<ComponentMetadata>,
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

export function clearDragState(
  model: EditorState,
  derived: DerivedState,
  applyChanges: boolean,
): EditorState {
  let result: EditorState = model
  if (applyChanges && result.canvas.dragState != null && result.canvas.dragState.drag != null) {
    const producedTransientCanvasState = produceCanvasTransientState(result, false)
    const producedTransientFileState = producedTransientCanvasState.fileState
    if (producedTransientFileState != null) {
      result = modifyOpenParseSuccess((success) => {
        let parseSuccessResult: ParseSuccess = {
          ...success,
          imports: producedTransientFileState.imports,
          topLevelElements: producedTransientFileState.topLevelElementsIncludingScenes,
        }
        return parseSuccessResult
      }, result)
    }
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
  components: Array<UtopiaJSXComponent>,
  metadata: Array<ComponentMetadata>,
  framesAndTargets: Array<PinOrFlexFrameChange>,
  optionalParentFrame: CanvasRectangle | null,
): Array<UtopiaJSXComponent> {
  let workingComponentsResult = [...components]
  Utils.fastForEach(framesAndTargets, (frameAndTarget) => {
    const { target } = frameAndTarget
    if (TP.isScenePath(target)) {
      switch (frameAndTarget.type) {
        case 'PIN_FRAME_CHANGE':
        case 'PIN_SIZE_CHANGE': {
          // Update scene with pin based frame.
          const sceneStaticpath = createSceneTemplatePath(target)
          workingComponentsResult = transformJSXComponentAtPath(
            workingComponentsResult,
            sceneStaticpath,
            (sceneElement) => {
              const sceneStyleUpdated = setJSXValuesAtPaths(sceneElement.props, [
                {
                  path: PP.create(['style']),
                  value: jsxAttributeValue({
                    left: frameAndTarget.frame?.x,
                    top: frameAndTarget.frame?.y,
                    width: frameAndTarget.frame?.width,
                    height: frameAndTarget.frame?.height,
                  }),
                },
              ])
              return foldEither(
                () => sceneElement,
                (updatedProps) => {
                  return {
                    ...sceneElement,
                    props: roundAttributeLayoutValues(updatedProps),
                  }
                },
                sceneStyleUpdated,
              )
            },
          )
          break
        }
        case 'PIN_MOVE_CHANGE': {
          const sceneStaticpath = createSceneTemplatePath(target)
          workingComponentsResult = transformJSXComponentAtPath(
            workingComponentsResult,
            sceneStaticpath,
            (sceneElement) => {
              const styleProps = jsxSimpleAttributeToValue(sceneElement.props['style'])
              if (isRight(styleProps)) {
                let frameProps: { [k: string]: string | number | undefined } = {}
                Utils.fastForEach(['PinnedLeft', 'PinnedTop'] as LayoutPinnedProp[], (p) => {
                  const framePoint = framePointForPinnedProp(p)
                  const value = getLayoutProperty(p, right(sceneElement.props))
                  if (isLeft(value) || value.value != null) {
                    frameProps[framePoint] = value.value
                  }
                })
                let propsToSet = getPropsToSetToMoveElement(
                  frameAndTarget.delta,
                  [FramePoint.Top, FramePoint.Left],
                  frameProps,
                  null,
                )
                const sceneStyleUpdated = setJSXValuesAtPaths(sceneElement.props, propsToSet)
                return foldEither(
                  () => sceneElement,
                  (updatedProps) => {
                    return {
                      ...sceneElement,
                      props: roundAttributeLayoutValues(updatedProps),
                    }
                  },
                  sceneStyleUpdated,
                )
              } else {
                return sceneElement
              }
            },
          )
          break
        }
        case 'SINGLE_RESIZE':
          const sceneStaticpath = createSceneTemplatePath(target)
          workingComponentsResult = transformJSXComponentAtPath(
            workingComponentsResult,
            sceneStaticpath,
            (sceneElement) => {
              const styleProps = jsxSimpleAttributeToValue(sceneElement.props['style'])
              if (isRight(styleProps)) {
                let frameProps: { [k: string]: string | number | undefined } = {}
                Utils.fastForEach(
                  ['PinnedLeft', 'PinnedTop', 'Width', 'Height'] as LayoutPinnedProp[],
                  (p) => {
                    const framePoint = framePointForPinnedProp(p)
                    const value = getLayoutProperty(p, right(sceneElement.props))
                    if (isLeft(value) || value.value != null) {
                      frameProps[framePoint] = value.value
                    }
                  },
                )
                let propsToSet = getPropsToSetToResizeElement(
                  frameAndTarget.edgePosition,
                  frameAndTarget.sizeDelta.x,
                  frameAndTarget.sizeDelta.y,
                  [FramePoint.Top, FramePoint.Left, FramePoint.Width, FramePoint.Height],
                  frameProps,
                  null,
                )
                const sceneStyleUpdated = setJSXValuesAtPaths(sceneElement.props, propsToSet)
                return foldEither(
                  () => sceneElement,
                  (updatedProps) => {
                    return {
                      ...sceneElement,
                      props: roundAttributeLayoutValues(updatedProps),
                    }
                  },
                  sceneStyleUpdated,
                )
              } else {
                return sceneElement
              }
            },
          )
          break
        case 'FLEX_MOVE':
        case 'FLEX_RESIZE':
          throw new Error(
            `Attempted to change a scene with a flex change ${JSON.stringify(target)}.`,
          )
        default:
          const _exhaustiveCheck: never = frameAndTarget
          throw new Error(`Unhandled type ${JSON.stringify(frameAndTarget)}`)
      }
    } else {
      // Realign to aim at the static version, not the dynamic one.
      const originalTarget = target
      const staticTarget = MetadataUtils.dynamicPathToStaticPath(metadata, target)
      if (staticTarget == null) {
        return
      }

      const element = findJSXElementAtPath(staticTarget, workingComponentsResult, metadata)
      if (element == null) {
        throw new Error(`Unexpected result when looking for element: ${element}`)
      }

      const staticParentPath = TP.parentPath(staticTarget)
      const parentElement = findJSXElementAtPath(
        staticParentPath,
        workingComponentsResult,
        metadata,
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
              workingComponentsResult = reorderComponent(
                workingComponentsResult,
                metadata,
                originalTarget,
                frameAndTarget.newIndex,
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
                  const { flexBasis, crossBasis } = flexProps
                  if (flexBasis != null) {
                    propsToSet.push({
                      path: createLayoutPropertyPath('FlexFlexBasis'),
                      value: jsxAttributeValue(flexBasis),
                    })
                  }
                  if (crossBasis != null) {
                    propsToSet.push({
                      path: createLayoutPropertyPath('FlexCrossBasis'),
                      value: jsxAttributeValue(crossBasis),
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
          const nonGroupParent = MetadataUtils.findNonGroupParent(metadata, originalTarget)
          parentFrame =
            nonGroupParent == null
              ? null
              : MetadataUtils.getFrameInCanvasCoords(nonGroupParent, metadata)
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
              const currentLocalFrame = MetadataUtils.getFrame(target, metadata)
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
                const previousValue =
                  currentFullFrame == null ? null : currentFullFrame[propToUpdate]

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
                    value: jsxAttributeValue(valueToUse),
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
              let horizontalPoints = framePointsToUse.filter((p) =>
                HorizontalFramePoints.includes(p),
              )

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
        workingComponentsResult = transformJSXComponentAtPath(
          workingComponentsResult,
          staticTarget,
          (e: JSXElement) => {
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
            const layoutPropsRemoved = unsetJSXValuesAtPaths(e.props, propsToRemove)
            // ...Add in the updated properties.

            const layoutPropsAdded = flatMapEither(
              (props) => setJSXValuesAtPaths(props, propsToSet),
              layoutPropsRemoved,
            )

            return foldEither(
              (_) => e,
              (updatedProps) => {
                return {
                  ...e,
                  props: updatedProps,
                }
              },
              layoutPropsAdded,
            )
          },
        )
      }

      // Round the frame details.
      workingComponentsResult = transformJSXComponentAtPath(
        workingComponentsResult,
        staticTarget,
        roundJSXElementLayoutValues,
      )
      // TODO originalFrames is never being set, so we have a regression here, meaning keepChildrenGlobalCoords
      // doesn't work. Once that is fixed we can re-implement keeping the children in place
    }
  })
  return workingComponentsResult
}

function updateFrameValueForProp(
  framePoint: FramePoint,
  delta: number,
  frameProps: { [k: string]: string | number | undefined },
  parentFrame: CanvasRectangle | null,
): ValueAtPath | null {
  if (delta !== 0) {
    const existingProp = frameProps[framePoint]
    const pinIsPercentage = existingProp == null ? false : isPercentPin(existingProp)
    let valueToUse: string | number
    if (existingProp != null && pinIsPercentage) {
      const percentValue = numberPartOfPin(existingProp)
      if (parentFrame != null) {
        const referenceSize = isHorizontalPoint(framePoint) ? parentFrame.width : parentFrame.height
        const deltaAsPercentValue = (delta / referenceSize) * 100
        valueToUse = `${percentValue + deltaAsPercentValue}%`
      } else {
        valueToUse = `${percentValue + delta}%`
      }
    } else {
      valueToUse = existingProp == null ? delta : Number(existingProp) + delta
    }
    return {
      path: createLayoutPropertyPath(pinnedPropForFramePoint(framePoint)),
      value: jsxAttributeValue(valueToUse),
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
  imports: Imports,
  metadata: Array<ComponentMetadata>,
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
  const imports = getOpenImportsFromState(editor)
  const guidelines = oneGuidelinePerDimension(
    collectGuidelines(
      imports,
      editor.jsxMetadataKILLME,
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
    editor.jsxMetadataKILLME,
    true,
    false,
  )
  const anythingPinnedAndNotAbsolutePositioned = elementsToTarget.some((elementToTarget) => {
    return MetadataUtils.isPinnedAndNotAbsolutePositioned(editor.jsxMetadataKILLME, elementToTarget)
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

export function produceResizeCanvasTransientState(
  editorState: EditorState,
  parseSuccess: ParseSuccess,
  dragState: ResizeDragState,
  preventAnimations: boolean,
): TransientCanvasState {
  const elementsToTarget = determineElementsToOperateOnForDragging(
    dragState.draggedElements,
    editorState.jsxMetadataKILLME,
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
          editorState.jsxMetadataKILLME,
        )
        if (isFlexContainer) {
          framesAndTargets.push(flexResizeChange(target, roundedFrame, dragState.edgePosition))
        } else {
          framesAndTargets.push(pinFrameChange(target, roundedFrame, dragState.edgePosition))
        }
      }
    })

    // We don't want animations included in the transient state, except for the case where we're about to apply that to the final state
    const untouchedOpenComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
    const openComponents = preventAnimations
      ? preventAnimationsOnTargets(
          untouchedOpenComponents,
          editorState.jsxMetadataKILLME,
          elementsToTarget,
        )
      : untouchedOpenComponents

    const componentsIncludingFakeUtopiaScene = openComponents

    const updatedScenesAndComponents = updateFramesOfScenesAndComponents(
      componentsIncludingFakeUtopiaScene,
      editorState.jsxMetadataKILLME,
      framesAndTargets,
      null,
    )

    // Sync these back up.
    const topLevelElementsIncludingScenes = applyUtopiaJSXComponentsChanges(
      parseSuccess.topLevelElements,
      updatedScenesAndComponents,
    )

    return transientCanvasState(
      editorState.selectedViews,
      editorState.highlightedViews,
      transientFileState(topLevelElementsIncludingScenes, parseSuccess.imports),
    )
  }
}

export function produceResizeSingleSelectCanvasTransientState(
  editorState: EditorState,
  parseSuccess: ParseSuccess,
  dragState: ResizeDragState,
  preventAnimations: boolean,
): TransientCanvasState {
  const elementsToTarget = determineElementsToOperateOnForDragging(
    dragState.draggedElements,
    editorState.jsxMetadataKILLME,
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
    const newTargetFrame = Utils.transformFrameUsingBoundingBox(newSize, globalFrame, originalFrame)
    const roundedFrame = {
      x: Math.floor(newTargetFrame.x),
      y: Math.floor(newTargetFrame.y),
      width: Math.ceil(newTargetFrame.width),
      height: Math.ceil(newTargetFrame.height),
    } as CanvasRectangle
    const isFlexContainer = MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      elementToTarget,
      editorState.jsxMetadataKILLME,
    )
    if (isFlexContainer) {
      framesAndTargets.push(flexResizeChange(elementToTarget, roundedFrame, dragState.edgePosition))
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
  }

  // We don't want animations included in the transient state, except for the case where we're about to apply that to the final state
  const untouchedOpenComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
  const openComponents = preventAnimations
    ? preventAnimationsOnTargets(
        untouchedOpenComponents,
        editorState.jsxMetadataKILLME,
        elementsToTarget,
      )
    : untouchedOpenComponents

  const componentsIncludingFakeUtopiaScene = openComponents

  const updatedScenesAndComponents = updateFramesOfScenesAndComponents(
    componentsIncludingFakeUtopiaScene,
    editorState.jsxMetadataKILLME,
    framesAndTargets,
    null,
  )

  // Sync these back up.
  const topLevelElementsIncludingScenes = applyUtopiaJSXComponentsChanges(
    parseSuccess.topLevelElements,
    updatedScenesAndComponents,
  )

  return transientCanvasState(
    editorState.selectedViews,
    editorState.highlightedViews,
    transientFileState(topLevelElementsIncludingScenes, parseSuccess.imports),
  )
}

export function produceCanvasTransientState(
  editorState: EditorState,
  preventAnimations: boolean,
): TransientCanvasState {
  function noFileTransientCanvasState(): TransientCanvasState {
    return transientCanvasState(editorState.selectedViews, editorState.highlightedViews, null)
  }
  function parseSuccessTransientCanvasState(parseSuccess: ParseSuccess): TransientCanvasState {
    const topLevelElementsIncludingScenes = parseSuccess.topLevelElements
    return {
      selectedViews: editorState.selectedViews,
      highlightedViews: editorState.highlightedViews,
      fileState: transientFileState(topLevelElementsIncludingScenes, parseSuccess.imports),
    }
  }
  const openUIFile = getOpenUIJSFile(editorState)
  if (openUIFile == null) {
    return noFileTransientCanvasState()
  } else {
    return foldEither(
      (_) => {
        return noFileTransientCanvasState()
      },
      (parseSuccess) => {
        switch (editorState.mode.type) {
          // When we're in insert mode, we need to temporarily slip the element being inserted into
          // the model to render.
          case 'insert':
            const insertMode = editorState.mode
            const openComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
            if (insertionSubjectIsJSXElement(insertMode.subject) && insertMode.insertionStarted) {
              const insertionParent =
                insertMode.subject.parent == null ? null : insertMode.subject.parent.staticTarget
              const updatedComponents = insertJSXElementChild(
                insertionParent,
                insertMode.subject.element,
                openComponents,
                {
                  type: 'front',
                },
              )
              const updatedImports: Imports = mergeImports(
                parseSuccess.imports,
                insertMode.subject.importsToAdd,
              )

              // Sync these back up.
              const topLevelElements = applyUtopiaJSXComponentsChanges(
                parseSuccess.topLevelElements,
                updatedComponents,
              )

              const topLevelElementsIncludingScenes = topLevelElements

              return transientCanvasState(
                editorState.selectedViews,
                editorState.highlightedViews,
                transientFileState(topLevelElementsIncludingScenes, updatedImports),
              )
            } else {
              return parseSuccessTransientCanvasState(parseSuccess)
            }
          case 'select':
            if (
              editorState.canvas.dragState == null ||
              editorState.canvas.dragState.start == null ||
              editorState.canvas.dragState.drag == null
            ) {
              return parseSuccessTransientCanvasState(parseSuccess)
            } else {
              const dragState = editorState.canvas.dragState
              switch (dragState.type) {
                case 'MOVE_DRAG_STATE':
                  return produceMoveTransientCanvasState(
                    editorState,
                    dragState,
                    parseSuccess,
                    preventAnimations,
                  )
                case 'RESIZE_DRAG_STATE':
                  if (dragState.isMultiSelect) {
                    return produceResizeCanvasTransientState(
                      editorState,
                      parseSuccess,
                      dragState,
                      preventAnimations,
                    )
                  } else {
                    return produceResizeSingleSelectCanvasTransientState(
                      editorState,
                      parseSuccess,
                      dragState,
                      preventAnimations,
                    )
                  }

                case 'INSERT_DRAG_STATE':
                  throw new Error(`Unable to use insert drag state in select mode.`)
                default:
                  const _exhaustiveCheck: never = dragState
                  throw new Error(`Unhandled drag state type ${JSON.stringify(dragState)}`)
              }
            }
          case 'live':
            return parseSuccessTransientCanvasState(parseSuccess)
          default:
            const _exhaustiveCheck: never = editorState.mode
            throw new Error(`Unhandled editor mode ${JSON.stringify(editorState.mode)}`)
        }
      },
      openUIFile.fileContents,
    )
  }
}

export function createDuplicationNewUIDsFromEditorState(
  editorState: EditorState,
): Array<DuplicateNewUID> {
  const rootComponents = getOpenUtopiaJSXComponentsFromState(editorState)
  return createDuplicationNewUIDs(
    editorState.selectedViews,
    editorState.jsxMetadataKILLME,
    rootComponents,
  )
}

export function createDuplicationNewUIDs(
  selectedViews: Array<TemplatePath>,
  componentMetadata: ComponentMetadata[],
  rootComponents: Array<UtopiaJSXComponent>,
): Array<DuplicateNewUID> {
  const targetViews = determineElementsToOperateOnForDragging(
    selectedViews,
    componentMetadata,
    true,
    false,
  )

  let existingIDs = getAllUniqueUids(rootComponents)

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
  editorState: EditorState,
  position: CanvasPoint,
): TemplatePath | undefined {
  const allTargets = Canvas.getAllTargetsAtPoint(
    editorState,
    position,
    [TargetSearchType.All],
    false,
    'loose',
  )
  // filtering for non-selected views from alltargets
  return R.head(
    allTargets.filter((target) =>
      editorState.selectedViews.every((view) => !TP.pathsEqual(view, target)),
    ),
  )
}

export function getReparentTarget(
  editorState: EditorState,
  toReparent: Array<TemplatePath>,
  position: CanvasPoint,
): {
  shouldReparent: boolean
  newParent: TemplatePath | null
} {
  const result = getReparentTargetAtPosition(editorState, position)
  const possibleNewParent = result == undefined ? null : result
  const currentParents = Utils.stripNulls(
    toReparent.map((view) => MetadataUtils.getParent(editorState.jsxMetadataKILLME, view)),
  )
  let parentSupportsChild = true
  if (possibleNewParent != null) {
    const imports = getOpenImportsFromState(editorState)
    parentSupportsChild = MetadataUtils.targetSupportsChildren(
      imports,
      editorState.jsxMetadataKILLME,
      possibleNewParent,
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
  utopiaComponentsIncludingScenes: Array<UtopiaJSXComponent>
  newPath: TemplatePath | null
  metadata: Array<ComponentMetadata>
  selectedViews: Array<TemplatePath>
  highlightedViews: Array<TemplatePath>
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
  target: TemplatePath,
  originalPath: TemplatePath,
  newFrame: CanvasRectangle | typeof SkipFrameChange | null,
  indexPosition: IndexPosition,
  newParentPath: TemplatePath | null,
  parentFrame: CanvasRectangle | null,
  utopiaComponentsIncludingScenes: Array<UtopiaJSXComponent>,
  componentMetadata: Array<ComponentMetadata>,
  selectedViews: Array<TemplatePath>,
  highlightedViews: Array<TemplatePath>,
  newParentLayoutSystem: LayoutSystem | null,
): MoveTemplateResult {
  function noChanges(): MoveTemplateResult {
    return {
      utopiaComponentsIncludingScenes: utopiaComponentsIncludingScenes,
      newPath: target,
      metadata: componentMetadata,
      selectedViews: selectedViews,
      highlightedViews: highlightedViews,
    }
  }
  if (TP.isScenePath(target)) {
    // We don't support scene re-parenting, so just update the frame
    if (newFrame === SkipFrameChange || newFrame == null) {
      return noChanges()
    } else {
      const updatedComponentsIncludingScenes = transformJSXComponentAtPath(
        utopiaComponentsIncludingScenes,
        createSceneTemplatePath(target),
        (sceneElement): JSXElement => {
          const updatedPropsResult = setJSXValueAtPath(
            sceneElement.props,
            PathForSceneStyle,
            jsxAttributeValue(canvasFrameToNormalisedFrame(newFrame)),
          )
          return foldEither(
            () => sceneElement,
            (updatedProps) => ({ ...sceneElement, props: updatedProps }),
            updatedPropsResult,
          )
        },
      )
      return {
        utopiaComponentsIncludingScenes: updatedComponentsIncludingScenes,
        newPath: target,
        metadata: componentMetadata,
        selectedViews: selectedViews,
        highlightedViews: highlightedViews,
      }
    }
  } else {
    let newIndex: number = 0
    let newInstancePath: InstancePath | null = null
    let flexContextChanged: boolean = false

    const targetID = TP.toTemplateId(target)
    if (newParentPath == null || TP.isScenePath(newParentPath) || TP.isScenePath(originalPath)) {
      // TODO Scene Implementation
      return noChanges()
    } else {
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
        utopiaComponentsIncludingScenes,
        parentFrame,
        newParentLayoutSystem,
      )

      const jsxElement = findElementAtPath(
        target,
        withLayoutUpdatedForNewContext,
        withMetadataUpdatedForNewContext,
      )
      if (jsxElement == null) {
        return noChanges()
      } else {
        let updatedUtopiaComponents: Array<UtopiaJSXComponent> = utopiaComponentsIncludingScenes
        let newPath: TemplatePath | null = null
        let updatedComponentMetadata: Array<ComponentMetadata> = withMetadataUpdatedForNewContext

        flexContextChanged = flexContextChanged || didSwitch

        const withTargetRemoved: Array<UtopiaJSXComponent> = removeElementAtPath(
          target,
          withLayoutUpdatedForNewContext,
          updatedComponentMetadata,
        )

        updatedUtopiaComponents = insertElementAtPath(
          newParentPath,
          jsxElement,
          withTargetRemoved,
          indexPosition,
          updatedComponentMetadata,
        )
        if (newParentPath == null) {
          newIndex = updatedUtopiaComponents.findIndex(
            (exported) => exported.rootElement === jsxElement,
          )
          if (newIndex === -1) {
            throw new Error('Invalid root element index.')
          }
        } else {
          const parent = findElementAtPath(
            newParentPath,
            updatedUtopiaComponents,
            updatedComponentMetadata,
          )
          if (parent == null || !isJSXElement(parent)) {
            throw new Error(`Couldn't find parent ${JSON.stringify(newParentPath)}`)
          } else {
            newIndex = parent.children.indexOf(jsxElement)
            if (newIndex === -1) {
              throw new Error('Invalid child element index.')
            }
          }
        }

        newPath = TP.appendToPath(newParentPath, targetID)
        newInstancePath = newPath

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

          updatedComponentMetadata = MetadataUtils.transformAllMetadata(
            updatedComponentMetadata,
            (metadata) => {
              const updatedPath = TP.replaceOrDefault(
                metadata.templatePath,
                target,
                newInstancePath,
              )
              return {
                ...metadata,
                templatePath: updatedPath,
              }
            },
          )
        }

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

          const frameChangeResult = updateFramesOfScenesAndComponents(
            updatedUtopiaComponents,
            updatedComponentMetadata,
            frameChanges,
            parentFrame,
          )

          updatedUtopiaComponents = frameChangeResult
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

        return {
          utopiaComponentsIncludingScenes: updatedUtopiaComponents,
          newPath: newInstancePath,
          metadata: updatedComponentMetadata,
          selectedViews: filterMultiSelectScenes(Utils.stripNulls(newSelectedViews)),
          highlightedViews: Utils.stripNulls(newHighlightedViews),
        }
      }
    }
  }
}

function preventAnimationsOnTargets(
  components: UtopiaJSXComponent[],
  metadata: ComponentMetadata[],
  targets: TemplatePath[],
): UtopiaJSXComponent[] {
  let workingComponentsResult = [...components]
  Utils.fastForEach(targets, (target) => {
    const staticPath = TP.isScenePath(target)
      ? createSceneTemplatePath(target)
      : MetadataUtils.dynamicPathToStaticPath(metadata, target)
    if (staticPath != null) {
      workingComponentsResult = transformJSXComponentAtPath(
        workingComponentsResult,
        staticPath,
        (element) => {
          const styleUpdated = setJSXValuesAtPaths(element.props, [
            {
              path: PP.create(['style', 'transition']),
              value: jsxAttributeValue('none'),
            },
          ])
          return foldEither(
            () => element,
            (updatedProps) => {
              return {
                ...element,
                props: updatedProps,
              }
            },
            styleUpdated,
          )
        },
      )
    }
  })
  return workingComponentsResult
}

function produceMoveTransientCanvasState(
  editorState: EditorState,
  dragState: MoveDragState,
  parseSuccess: ParseSuccess,
  preventAnimations: boolean,
): TransientCanvasState {
  let selectedViews: Array<TemplatePath> = dragState.draggedElements
  let metadata: Array<ComponentMetadata> = editorState.jsxMetadataKILLME
  let originalFrames: Array<CanvasFrameAndTarget> = dragState.originalFrames
  let highlightedViews: Array<TemplatePath> = editorState.highlightedViews

  const elementsToTarget = determineElementsToOperateOnForDragging(
    selectedViews,
    metadata,
    true,
    false,
  )

  // We don't want animations included in the transient state, except for the case where we're about to apply that to the final state
  const untouchedOpenComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
  let utopiaComponentsIncludingScenes = preventAnimations
    ? preventAnimationsOnTargets(
        untouchedOpenComponents,
        editorState.jsxMetadataKILLME,
        elementsToTarget,
      )
    : untouchedOpenComponents

  if (dragState.reparent) {
    const reparentTarget = getReparentTarget(
      editorState,
      elementsToTarget,
      dragState.canvasPosition,
    )

    if (reparentTarget.shouldReparent) {
      Utils.fastForEach(elementsToTarget, (target) => {
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
          utopiaComponentsIncludingScenes,
          metadata,
          selectedViews,
          highlightedViews,
          null,
        )
        highlightedViews = reparentResult.highlightedViews
        selectedViews = reparentResult.selectedViews
        metadata = reparentResult.metadata
        utopiaComponentsIncludingScenes = reparentResult.utopiaComponentsIncludingScenes
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
      })
    }
  } else if (dragState.duplicate) {
    const parentTarget = MetadataUtils.getDuplicationParentTargets(selectedViews)
    const duplicateResult = duplicate(elementsToTarget, parentTarget, editorState)
    if (duplicateResult != null) {
      utopiaComponentsIncludingScenes = duplicateResult.utopiaComponents
      selectedViews = duplicateResult.selectedViews
      metadata = duplicateResult.metadata
      if (duplicateResult.originalFrames != null) {
        originalFrames = duplicateResult.originalFrames
      }
    }
  }

  const moveGuidelines = collectParentAndSiblingGuidelines(metadata, selectedViews)
  const framesAndTargets = dragComponent(
    metadata,
    selectedViews,
    originalFrames,
    moveGuidelines,
    dragState.dragSelectionBoundingBox,
    dragState.drag,
    Utils.defaultIfNull(Utils.zeroPoint as CanvasPoint, dragState.drag),
    dragState.enableSnapping,
    dragState.constrainDragAxis,
    editorState.canvas.scale,
  )

  const componentsIncludingFakeUtopiaScene = utopiaComponentsIncludingScenes

  const updatedScenesAndComponents = updateFramesOfScenesAndComponents(
    componentsIncludingFakeUtopiaScene,
    metadata,
    framesAndTargets,
    null,
  )

  // Sync these back up.
  const topLevelElementsIncludingScenes = applyUtopiaJSXComponentsChanges(
    parseSuccess.topLevelElements,
    updatedScenesAndComponents,
  )
  return transientCanvasState(
    selectedViews,
    highlightedViews,
    transientFileState(topLevelElementsIncludingScenes, parseSuccess.imports),
  )
}

export function getCanvasOffset(
  previousOffset: CanvasPoint,
  previousScale: number,
  scale: number,
  componentMetadata: ComponentMetadata[],
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
  componentMetadata: ComponentMetadata[],
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
  utopiaComponents: Array<UtopiaJSXComponent>
  selectedViews: Array<TemplatePath>
  metadata: Array<ComponentMetadata>
  originalFrames: Array<CanvasFrameAndTarget> | null
}

export function duplicate(
  paths: Array<TemplatePath>,
  newParentPath: TemplatePath | null,
  editor: EditorState,
): DuplicateResult | null {
  const uiFile = getOpenUIJSFile(editor)
  if (uiFile == null) {
    return null
  } else {
    return foldEither(
      (_) => null,
      (parseSuccess) => {
        let metadata = editor.jsxMetadataKILLME
        let utopiaComponents = getUtopiaJSXComponentsFromSuccess(parseSuccess)
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
        for (const path of paths) {
          let newElement: JSXElementChild | null = null
          let jsxElement: JSXElementChild | null = null
          if (TP.isScenePath(path)) {
            const scenepath = createSceneTemplatePath(path)
            jsxElement = findJSXElementChildAtPath(utopiaComponents, scenepath)
          } else {
            jsxElement = findElementAtPath(path, utopiaComponents, metadata)
          }
          let uid: string
          if (jsxElement == null) {
            console.warn(`Could not find element ${TP.toVarSafeComponentId(path)}`)
            return null
          } else {
            const duplicateNewUID: DuplicateNewUID | undefined = duplicateNewUIDs.find((entry) =>
              TP.pathsEqual(entry.originalPath, path),
            )
            if (duplicateNewUID === undefined) {
              const existingIDs = getAllUniqueUids(utopiaComponents)
              newElement = guaranteeUniqueUids([jsxElement], existingIDs)[0]
              uid = getUtopiaID(newElement)
            } else {
              // Helps to keep the model consistent because otherwise the dom walker
              // goes into a frenzy.
              newElement = setUtopiaID(jsxElement, duplicateNewUID.newUID)
              uid = duplicateNewUID.newUID
            }
            let newPath: TemplatePath
            if (newParentPath == null) {
              const storyboardUID = Utils.forceNotNull(
                'Could not find storyboard element',
                getStoryboardUID(utopiaComponents),
              )
              newPath = TP.scenePath([storyboardUID, uid])
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
              const props = {
                ...jsxElement.props,
                'data-label': jsxAttributeValue(newSceneLabel),
                'data-uid': jsxAttributeValue(getUtopiaID(newElement)),
              }
              const newSceneElement = {
                ...jsxElement,
                props: props,
              }
              utopiaComponents = addSceneToJSXComponents(utopiaComponents, newSceneElement)
            } else {
              utopiaComponents = insertElementAtPath(
                newParentPath,
                newElement,
                utopiaComponents,
                null,
                metadata,
              )
            }

            if (newElement == null) {
              console.warn(`Could not duplicate ${TP.toVarSafeComponentId(path)}`)
              return null
            } else {
              newSelectedViews.push(newPath)
              // duplicating and inserting the metadata to ensure we're not working with stale metadata
              // this is used for drag + duplicate on the canvas
              metadata = MetadataUtils.duplicateElementMetadataAtPath(
                path,
                newPath,
                right(newElement),
                metadata,
              )
            }
          }
        }

        return {
          utopiaComponents: utopiaComponents,
          metadata: metadata,
          selectedViews: newSelectedViews,
          originalFrames: newOriginalFrames,
        }
      },
      uiFile.fileContents,
    )
  }
}

export function reorderComponent(
  components: Array<UtopiaJSXComponent>,
  componentMetadata: Array<ComponentMetadata>,
  target: InstancePath,
  newIndex: number,
): Array<UtopiaJSXComponent> {
  let workingComponents = [...components]

  const parentPath = TP.parentPath(target)
  const jsxElement = findElementAtPath(target, workingComponents, componentMetadata)

  if (jsxElement != null) {
    const newPosition: IndexPosition = {
      type: 'absolute',
      index: newIndex,
    }

    workingComponents = removeElementAtPath(target, workingComponents, componentMetadata)

    workingComponents = insertElementAtPath(
      parentPath,
      jsxElement,
      workingComponents,
      newPosition,
      componentMetadata,
    )
  }

  return workingComponents
}

export function createTestProjectWithCode(appUiJsFile: string): PersistentModel {
  const baseModel = defaultProject()
  const parsedFile = lintAndParse('/src/app.js', appUiJsFile) as ParseResult

  if (isParseFailure(parsedFile)) {
    fail('The test file parse failed')
  }

  return {
    ...baseModel,
    projectContents: {
      ...baseModel.projectContents,
      '/src/app.js': uiJsFile(parsedFile, null, RevisionsState.BothMatch, Date.now()),
    },
    selectedFile: openFileTab('/src/app.js'),
  }
}

export function cullSpyCollector(
  spyCollector: UiJsxCanvasContextData,
  domMetadata: Array<ElementInstanceMetadata>,
): void {
  // Note: Mutates `spyCollector`.
  // Collate all the valid paths.
  let elementPaths: Set<string> = Utils.emptySet()
  let scenePaths: Set<string> = Utils.emptySet()
  fastForEach(domMetadata, (rootMetadata) => {
    MetadataUtils.walkElementMetadata(
      rootMetadata,
      null,
      (metadata: ElementInstanceMetadata, parentMetadata: ElementInstanceMetadata | null) => {
        let workingPath: TemplatePath | null = metadata.templatePath
        while (workingPath != null) {
          const pathAsString = TP.toString(workingPath)
          if (TP.isScenePath(workingPath)) {
            elementPaths.add(TP.toString(TP.instancePath([], workingPath.sceneElementPath)))
            scenePaths.add(pathAsString)
          } else {
            elementPaths.add(pathAsString)
          }
          workingPath = TP.parentPath(workingPath)
        }
      },
    )
  })
  // Eliminate the element paths which are invalid.
  fastForEach(Object.keys(spyCollector.current.spyValues.metadata), (elementPath) => {
    if (!elementPaths.has(elementPath)) {
      delete spyCollector.current.spyValues.metadata[elementPath]
    }
  })
  // Eliminate the scene paths which are invalid.
  fastForEach(Object.keys(spyCollector.current.spyValues.scenes), (scenePath) => {
    if (!scenePaths.has(scenePath)) {
      delete spyCollector.current.spyValues.scenes[scenePath]
    }
  })
}
