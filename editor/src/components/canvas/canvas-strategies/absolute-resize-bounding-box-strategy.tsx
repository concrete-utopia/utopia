import { isHorizontalPoint } from 'utopia-api/core'
import { getLayoutProperty } from '../../../core/layout/getLayoutProperty'
import { framePointForPinnedProp } from '../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../core/shared/array-utils'
import { isRight, right } from '../../../core/shared/either'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import {
  CanvasRectangle,
  rectangleDifference,
  roundTo,
  transformFrameUsingBoundingBox,
} from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { AllElementProps, getElementFromProjectContents } from '../../editor/store/editor-state'
import { stylePropPathMappingFn } from '../../inspector/common/property-path-hooks'
import { EdgePosition } from '../canvas-types'
import {
  AdjustCssLengthProperty,
  adjustCssLengthProperty,
} from '../commands/adjust-css-length-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { AbsoluteResizeControl } from '../controls/select-mode/absolute-resize-control'
import { AbsolutePin, ensureAtLeastTwoPinsForEdgePosition } from './absolute-resize-helpers'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { getDragTargets, getMultiselectBounds } from './shared-absolute-move-strategy-helpers'
import {
  pickCursorFromEdgePosition,
  resizeBoundingBox,
  runLegacyAbsoluteResizeSnapping,
} from './shared-absolute-resize-strategy-helpers'
import * as EP from '../../../core/shared/element-path'
import { ZeroSizeResizeControlWrapper } from '../controls/zero-sized-element-controls'
import { SetCssLengthProperty, setCssLengthProperty } from '../commands/set-css-length-command'
import { pushIntendedBounds } from '../commands/push-intended-bounds-command'

export const absoluteResizeBoundingBoxStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_RESIZE_BOUNDING_BOX',
  name: 'Absolute Resize',
  isApplicable: (canvasState, interactionState, metadata, allElementProps) => {
    if (canvasState.selectedElements.length > 0) {
      const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
      return filteredSelectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    { control: AbsoluteResizeControl, key: 'absolute-resize-control', show: 'always-visible' },
    {
      control: ZeroSizeResizeControlWrapper,
      key: 'zero-size-resize-control',
      show: 'always-visible',
    },
    { control: ParentOutlines, key: 'parent-outlines-control', show: 'visible-only-while-active' },
    { control: ParentBounds, key: 'parent-bounds-control', show: 'visible-only-while-active' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    return absoluteResizeBoundingBoxStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
      sessionState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
    ) {
      const edgePosition = interactionState.activeControl.edgePosition
      if (interactionState.interactionData.drag != null) {
        const drag = interactionState.interactionData.drag
        const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
        const originalBoundingBox = getMultiselectBounds(
          sessionState.startingMetadata,
          filteredSelectedElements,
        )
        if (originalBoundingBox != null) {
          const keepAspectRatio = interactionState.interactionData.modifiers.shift
          const lockedAspectRatio = keepAspectRatio
            ? originalBoundingBox.width / originalBoundingBox.height
            : null
          const centerBased = interactionState.interactionData.modifiers.alt
            ? 'center-based'
            : 'non-center-based'
          const newBoundingBox = resizeBoundingBox(
            originalBoundingBox,
            drag,
            edgePosition,
            lockedAspectRatio,
            centerBased,
          )
          const { snappedBoundingBox, guidelinesWithSnappingVector } = snapBoundingBox(
            filteredSelectedElements,
            sessionState.startingMetadata,
            edgePosition,
            newBoundingBox,
            canvasState.scale,
            lockedAspectRatio,
            centerBased,
            sessionState.startingAllElementProps,
          )

          const commandsForSelectedElements = filteredSelectedElements.flatMap(
            (selectedElement) => {
              const element = getElementFromProjectContents(
                selectedElement,
                canvasState.projectContents,
                canvasState.openFile,
              )
              const originalFrame = MetadataUtils.getFrameInCanvasCoords(
                selectedElement,
                sessionState.startingMetadata,
              )

              if (element == null || originalFrame == null) {
                return []
              }

              const newFrame = transformFrameUsingBoundingBox(
                snappedBoundingBox,
                originalBoundingBox,
                originalFrame,
              )
              const elementParentBounds =
                MetadataUtils.findElementByElementPath(
                  sessionState.startingMetadata,
                  selectedElement,
                )?.specialSizeMeasurements.immediateParentBounds ?? null

              return [
                ...createResizeCommandsFromFrame(
                  element,
                  selectedElement,
                  newFrame,
                  originalFrame,
                  elementParentBounds,
                  edgePosition,
                ),
                setSnappingGuidelines('transient', guidelinesWithSnappingVector), // TODO I think this will override the previous snapping guidelines
                pushIntendedBounds([{ target: selectedElement, frame: newFrame }]),
              ]
            },
          )
          return {
            commands: [
              ...commandsForSelectedElements,
              updateHighlightedViews('transient', []),
              setCursorCommand('transient', pickCursorFromEdgePosition(edgePosition)),
              setElementsToRerenderCommand(canvasState.selectedElements),
            ],
            customState: null,
          }
        }
      } else {
        return {
          commands: [
            setCursorCommand('transient', pickCursorFromEdgePosition(edgePosition)),
            updateHighlightedViews('transient', []),
          ],
          customState: null,
        }
      }
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

function createResizeCommandsFromFrame(
  element: JSXElement,
  selectedElement: ElementPath,
  newFrame: CanvasRectangle,
  originalFrame: CanvasRectangle,
  elementParentBounds: CanvasRectangle | null,
  edgePosition: EdgePosition,
): (AdjustCssLengthProperty | SetCssLengthProperty)[] {
  const pins: Array<AbsolutePin> = ensureAtLeastTwoPinsForEdgePosition(
    right(element.props),
    edgePosition,
  )
  return mapDropNulls((pin) => {
    const horizontal = isHorizontalPoint(
      // TODO avoid using the loaded FramePoint enum
      framePointForPinnedProp(pin),
    )
    const value = getLayoutProperty(pin, right(element.props), ['style'])
    const rectangleDiff = rectangleDifference(originalFrame, newFrame)
    const delta = allPinsFromFrame(rectangleDiff)[pin]
    const roundedDelta = roundTo(delta, 0)
    const pinDirection = pin === 'right' || pin === 'bottom' ? -1 : 1
    if (roundedDelta !== 0) {
      if (isRight(value) && value.value != null) {
        return adjustCssLengthProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn(pin, ['style']),
          roundedDelta * pinDirection,
          horizontal ? elementParentBounds?.width : elementParentBounds?.height,
          true,
        )
      } else {
        const valueToSet = allPinsFromFrame(newFrame)[pin]
        return setCssLengthProperty(
          'permanent',
          selectedElement,
          stylePropPathMappingFn(pin, ['style']),
          roundTo(valueToSet, 0),
          horizontal ? elementParentBounds?.width : elementParentBounds?.height,
        )
      }
    } else {
      return null
    }
  }, pins)
}

function allPinsFromFrame(frame: CanvasRectangle): { [key: string]: number } {
  return {
    left: frame.x,
    top: frame.y,
    width: frame.width,
    height: frame.height,
    right: frame.x + frame.width,
    bottom: frame.y + frame.height,
  }
}

function snapBoundingBox(
  selectedElements: Array<ElementPath>,
  jsxMetadata: ElementInstanceMetadataMap,
  edgePosition: EdgePosition,
  resizedBounds: CanvasRectangle,
  canvasScale: number,
  lockedAspectRatio: number | null,
  centerBased: 'center-based' | 'non-center-based',
  allElementProps: AllElementProps,
) {
  const { snappedBoundingBox, guidelinesWithSnappingVector } = runLegacyAbsoluteResizeSnapping(
    selectedElements,
    jsxMetadata,
    edgePosition,
    resizedBounds,
    canvasScale,
    lockedAspectRatio,
    centerBased,
    allElementProps,
  )

  return {
    snappedBoundingBox,
    guidelinesWithSnappingVector,
  }
}
