import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import {
  LayoutPinnedProp,
  LayoutPinnedProps,
  LayoutTargetableProp,
} from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { safeIndex } from '../../../core/shared/array-utils'
import { foldEither, isLeft, right } from '../../../core/shared/either'
import {
  ElementInstanceMetadataMap,
  emptyComments,
  jsxAttributeValue,
  JSXElement,
} from '../../../core/shared/element-template'
import { setJSXValuesAtPaths, ValueAtPath } from '../../../core/shared/jsx-attributes'
import {
  canvasPoint,
  CanvasPoint,
  CanvasRectangle,
  CanvasVector,
} from '../../../core/shared/math-utils'
import { optionalMap } from '../../../core/shared/optional-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { fastForEach } from '../../../core/shared/utils'
import { keepDeepReferenceEqualityIfPossible } from '../../../utils/react-performance'
import Utils from '../../../utils/utils'
import { isAspectRatioLockedNew } from '../../aspect-ratio'
import {
  EditorState,
  forUnderlyingTargetFromEditorState,
  modifyUnderlyingForOpenFile,
  TransientFilesState,
  withUnderlyingTargetFromEditorState,
} from '../../editor/store/editor-state'
import * as Frame from '../../frame'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import {
  EdgePosition,
  PinFrameChange,
  pinFrameChange,
  resizeDragStatePropertyChange,
  updateResizeDragStatePropertyChange,
} from '../canvas-types'
import {
  extendPartialFramePointsForResize,
  findResizePropertyChange,
  getOriginalFrameInCanvasCoords,
  isEdgePositionOnSide,
  pickPointOnRect,
  snapPoint,
  valueToUseForPin,
} from '../canvas-utils'
import { determineElementsToOperateOnForDragging } from '../controls/select-mode/move-utils'
import {
  AbsoluteResizeStrategyState,
  CanvasStrategy,
  CanvasStrategyUpdateFnResult,
  emptyAbsoluteResizeStrategyState,
  ResizeHandle,
  SelectModeCanvasSessionProps,
  SelectModeCanvasSessionState,
} from './canvas-strategy-types'

function calculateDraggedRectangle(
  editor: EditorState,
  resizeHandle: ResizeHandle,
  sessionProps: SelectModeCanvasSessionProps,
  sessionState: SelectModeCanvasSessionState,
): CanvasRectangle {
  const originalSize = resizeHandle.originalSize
  const resizeOptions = editor.canvas.resizeOptions

  const propertyChange = findResizePropertyChange(
    sessionState.absoluteResizeStrategy?.properties ?? [],
    resizeOptions,
  )
  if (propertyChange == null) {
    return originalSize
  } else {
    // for center based resize, we need to calculate with double deltas
    // for now, because scale is not a first-class citizen, we know that CanvasVector and LocalVector have the same dimensions
    // this will break with the introduction of scale into the coordinate systems
    const deltaScale = propertyChange.centerBasedResize ? 2 : 1
    let delta: CanvasVector = canvasPoint({ x: 0, y: 0 })
    const drag = sessionProps.drag
    if (drag != null) {
      delta = Utils.scaleVector(
        Utils.scalePoint(drag, resizeHandle.enabledDirection as CanvasVector),
        deltaScale,
      )
    }
    const startingCorner: EdgePosition = {
      x: 1 - resizeHandle.edgePosition.x,
      y: 1 - resizeHandle.edgePosition.y,
    } as EdgePosition
    const startingPoint = pickPointOnRect(originalSize, startingCorner)
    const originalCenter = Utils.getRectCenter(originalSize)
    const draggedCorner = pickPointOnRect(originalSize, resizeHandle.edgePosition)

    const newCorner = Utils.offsetPoint(draggedCorner, delta)
    const snappedNewCorner = Utils.roundPointTo(
      snapPoint(
        editor,
        newCorner,
        propertyChange.enableSnapping,
        propertyChange.keepAspectRatio,
        startingPoint,
        draggedCorner,
        startingCorner,
      ),
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
  resizeHandle: ResizeHandle,
  sessionProps: SelectModeCanvasSessionProps,
  sessionState: SelectModeCanvasSessionState,
): CanvasRectangle {
  const originalSize = resizeHandle.originalSize
  const aspectRatio = originalSize.width / originalSize.height
  const newRectangle = calculateDraggedRectangle(editor, resizeHandle, sessionProps, sessionState)
  const resizeOptions = editor.canvas.resizeOptions

  const propertyChange = findResizePropertyChange(
    sessionState.absoluteResizeStrategy?.properties ?? [],
    resizeOptions,
  )
  if (propertyChange == null) {
    return originalSize
  } else {
    // At this point I do ugly things to keep side drags in line
    if (resizeHandle.edgePosition.x === 0.5) {
      const newWidth = propertyChange.keepAspectRatio
        ? Utils.roundTo(newRectangle.height * aspectRatio)
        : originalSize.width
      newRectangle.x -= newWidth / 2
      newRectangle.width = newWidth
    }
    if (resizeHandle.edgePosition.y === 0.5) {
      const newHeight = propertyChange.keepAspectRatio
        ? Utils.roundTo(newRectangle.width / aspectRatio)
        : originalSize.height
      newRectangle.y -= newHeight / 2
      newRectangle.height = newHeight
    }

    return newRectangle
  }
}

function applyResize(
  jsxMetadata: ElementInstanceMetadataMap,
  frameAndTarget: PinFrameChange,
  element: JSXElement,
): Array<ValueAtPath> {
  let propsToSet: Array<ValueAtPath> = []
  let propsToSkip: Array<PropertyPath> = []
  const nonGroupParent = MetadataUtils.findParent(jsxMetadata, frameAndTarget.target)
  const parentFrame =
    nonGroupParent == null
      ? null
      : MetadataUtils.getFrameInCanvasCoords(nonGroupParent, jsxMetadata)
  const parentOffset =
    parentFrame == null
      ? ({ x: 0, y: 0 } as CanvasPoint)
      : ({ x: parentFrame.x, y: parentFrame.y } as CanvasPoint)

  if (frameAndTarget.frame != null) {
    const newLocalFrame = Utils.getLocalRectangleInNewParentContext(
      parentOffset,
      frameAndTarget.frame,
    )
    const fullFrame = Frame.getFullFrame(newLocalFrame)
    const elementProps = element.props

    // Pinning layout.
    const frameProps = LayoutPinnedProps.filter((p) => {
      const value = getLayoutProperty(p, right(elementProps), ['style'])
      return isLeft(value) || value.value != null
    })

    function whichPropsToUpdate(): Array<LayoutPinnedProp> {
      if (
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

      const propPathToUpdate = stylePropPathMappingFn(propToUpdate, ['style'])
      const existingProp = getLayoutProperty(propToUpdate, right(elementProps), ['style'])
      if (isLeft(existingProp)) {
        // Only update pins that aren't set via code
        propsToSkip.push(propPathToUpdate)
      } else {
        const pinIsPercentage = existingProp.value == null ? false : existingProp.value.unit === '%'
        let valueToUse: string | number
        if (parentFrame == null) {
          valueToUse = absoluteValue
        } else {
          valueToUse = valueToUseForPin(propToUpdate, absoluteValue, pinIsPercentage, parentFrame)
        }
        propsToSet.push({
          path: propPathToUpdate,
          value: jsxAttributeValue(valueToUse, emptyComments),
        })
      }
    })
  }

  return propsToSet
}

function updateResizeStrategyState(
  current: AbsoluteResizeStrategyState,
  startForNewProperties: CanvasPoint,
  drag: CanvasVector | null,
  targetProperty: LayoutTargetableProp | undefined,
  enableSnapping: boolean,
  centerBasedResize: boolean,
  keepAspectRatio: boolean,
): AbsoluteResizeStrategyState {
  let propertyAlreadyExists: boolean = false
  let updatedProperties = current.properties.map((prop) => {
    if (prop.targetProperty === targetProperty) {
      propertyAlreadyExists = true
      return updateResizeDragStatePropertyChange(
        prop,
        drag,
        targetProperty,
        enableSnapping,
        centerBasedResize,
        keepAspectRatio,
      )
    } else {
      return prop
    }
  })
  if (!propertyAlreadyExists) {
    updatedProperties = [
      ...current.properties,
      resizeDragStatePropertyChange(
        startForNewProperties,
        drag,
        enableSnapping,
        centerBasedResize,
        keepAspectRatio,
        targetProperty,
      ),
    ]
  }
  return keepDeepReferenceEqualityIfPossible(current, {
    ...current,
    properties: updatedProperties,
  })
}

export const resizeAbsoluteStrategy: CanvasStrategy = {
  name: 'Resize Absolute Elements',
  fitnessFn: (editor, currentSession) => {
    if (editor.selectedViews.length === 0) {
      return null
    }
    for (const selectedView of editor.selectedViews) {
      const isAbsolute = MetadataUtils.isPositionAbsolute(
        MetadataUtils.findElementByElementPath(editor.jsxMetadata, selectedView),
      )
      if (!isAbsolute) {
        return null
      }
    }
    // More than one selected view which is absolute positioned, so this should work.
    return 10
  },
  updateFn: (
    lifecycle: 'transient' | 'final',
    editorState: EditorState,
    sessionProps: SelectModeCanvasSessionProps,
    sessionState: SelectModeCanvasSessionState,
  ): CanvasStrategyUpdateFnResult => {
    if (sessionProps.activeControl.type === 'RESIZE_HANDLE') {
      const resizeHandle = sessionProps.activeControl

      const elementsToTarget = determineElementsToOperateOnForDragging(
        editorState.selectedViews,
        editorState.jsxMetadata,
        false,
        true,
      )

      const newSize = calculateNewBounds(editorState, resizeHandle, sessionProps, sessionState)
      let framesAndTargets: Array<PinFrameChange> = []
      let globalFrames: Array<CanvasRectangle> = []
      fastForEach(editorState.selectedViews, (selectedView) => {
        const frame = getOriginalFrameInCanvasCoords(resizeHandle.originalFrames, selectedView)
        if (frame != null) {
          globalFrames.push(frame)
        }
      })
      const boundingBox = Utils.boundingRectangleArray(globalFrames)
      if (boundingBox != null) {
        Utils.fastForEach(elementsToTarget, (target) => {
          const originalFrame = getOriginalFrameInCanvasCoords(resizeHandle.originalFrames, target)
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

            framesAndTargets.push(pinFrameChange(target, roundedFrame, resizeHandle.edgePosition))
          }
        })

        let workingTransientFilesState: TransientFilesState = {}
        let workingEditorState: EditorState = { ...editorState }
        const preventAnimations = true
        if (preventAnimations) {
          const transitionValueToSet = [
            {
              path: PP.create(['style', 'transition']),
              value: jsxAttributeValue('none', emptyComments),
            },
          ]
          for (const elementToTarget of elementsToTarget) {
            const withAnimationsPrevented = applyValuesAtPath(
              workingEditorState,
              workingTransientFilesState,
              elementToTarget,
              transitionValueToSet,
            )
            workingTransientFilesState = withAnimationsPrevented.transientFilesState
            workingEditorState = withAnimationsPrevented.editorState
          }
        }

        for (const frameAndTarget of framesAndTargets) {
          const propsToSet = withUnderlyingTargetFromEditorState(
            frameAndTarget.target,
            workingEditorState,
            [],
            (_success, element) => {
              return applyResize(workingEditorState.jsxMetadata, frameAndTarget, element)
            },
          )
          const withUpdates = applyValuesAtPath(
            workingEditorState,
            workingTransientFilesState,
            frameAndTarget.target,
            propsToSet,
          )
          workingEditorState = withUpdates.editorState
          workingTransientFilesState = withUpdates.transientFilesState
        }

        const absoluteStrategy =
          sessionState.absoluteResizeStrategy ?? emptyAbsoluteResizeStrategyState
        const targetProperty = safeIndex(
          absoluteStrategy.properties,
          absoluteStrategy.propertyTargetSelectedIndex,
        )

        const elementAspectRatioLockedFromProps = elementsToTarget.every((elementToTarget) => {
          const metadata = MetadataUtils.findElementByElementPath(
            workingEditorState.jsxMetadata,
            elementToTarget,
          )
          if (metadata == null) {
            return false
          } else {
            return isAspectRatioLockedNew(metadata)
          }
        })

        const keepAspectRatio =
          (editorState.keysPressed['shift'] ?? false) || elementAspectRatioLockedFromProps
        const centerBasedResize = editorState.keysPressed['alt'] ?? false
        const enableSnapping = editorState.keysPressed['cmd'] ?? false

        const newStrategyState = updateResizeStrategyState(
          absoluteStrategy,
          sessionProps.start,
          sessionProps.drag,
          targetProperty?.targetProperty,
          enableSnapping,
          centerBasedResize,
          keepAspectRatio,
        )
        const newSessionState: SelectModeCanvasSessionState = {
          ...sessionState,
          absoluteResizeStrategy: newStrategyState,
        }

        return {
          newSessionState: newSessionState,
          transientFilesState: workingTransientFilesState,
          editorStatePatch: {},
        }
      }
    }

    // Fallback for when the checks above are not satisfied.
    return {
      newSessionState: sessionState,
      transientFilesState: {},
      editorStatePatch: {},
    }
  },
}

function applyValuesAtPath(
  editorState: EditorState,
  filesState: TransientFilesState,
  target: ElementPath,
  jsxValuesAndPathsToSet: ValueAtPath[],
): { editorState: EditorState; transientFilesState: TransientFilesState } {
  let workingEditorState = { ...editorState }
  let transientFilesState = { ...filesState }

  workingEditorState = modifyUnderlyingForOpenFile(target, editorState, (element: JSXElement) => {
    return foldEither(
      () => {
        return element
      },
      (updatedProps) => {
        return {
          ...element,
          props: updatedProps,
        }
      },
      setJSXValuesAtPaths(element.props, jsxValuesAndPathsToSet),
    )
  })

  forUnderlyingTargetFromEditorState(
    target,
    workingEditorState,
    (success, underlyingElement, underlyingTarget, underlyingFilePath) => {
      transientFilesState[underlyingFilePath] = {
        topLevelElementsIncludingScenes: success.topLevelElements,
        imports: success.imports,
      }
      return success
    },
  )
  return { editorState: workingEditorState, transientFilesState: transientFilesState }
}
