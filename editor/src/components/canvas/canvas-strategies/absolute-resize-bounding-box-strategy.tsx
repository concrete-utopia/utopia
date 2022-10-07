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
import {
  AbsolutePin,
  ensureAtLeastTwoPinsForEdgePosition,
  supportsAbsoluteResize,
} from './absolute-resize-helpers'
import {
  CanvasStrategy,
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getDragTargets, getMultiselectBounds } from './shared-move-strategies-helpers'
import {
  pickCursorFromEdgePosition,
  resizeBoundingBox,
  runLegacyAbsoluteResizeSnapping,
} from './shared-absolute-resize-strategy-helpers'
import * as EP from '../../../core/shared/element-path'
import { ZeroSizeResizeControlWrapper } from '../controls/zero-sized-element-controls'
import { SetCssLengthProperty, setCssLengthProperty } from '../commands/set-css-length-command'
import { pushIntendedBounds } from '../commands/push-intended-bounds-command'
import { InteractionSession } from './interaction-state'
import { Modifiers } from '../../../utils/modifiers'

export const absoluteResizeBoundingBoxStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_RESIZE_BOUNDING_BOX',
  name: () => 'Resize',
  isApplicable: (canvasState, interactionSession, metadata, allElementProps) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length > 0) {
      const filteredSelectedElements = getDragTargets(selectedElements)
      return filteredSelectedElements.every((element) => {
        return supportsAbsoluteResize(metadata, element, canvasState)
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    controlWithProps({
      control: AbsoluteResizeControl,
      props: {},
      key: 'absolute-resize-control',
      show: 'visible-except-when-other-strategy-is-active',
    }),
    controlWithProps({
      control: ZeroSizeResizeControlWrapper,
      props: {},
      key: 'zero-size-resize-control',
      show: 'visible-except-when-other-strategy-is-active',
    }),
    controlWithProps({
      control: ParentOutlines,
      props: {},
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    }),
    controlWithProps({
      control: ParentBounds,
      props: {},
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    }),
  ],
  fitness: (canvasState, interactionSession, customStrategyState) => {
    return absoluteResizeBoundingBoxStrategy.isApplicable(
      canvasState,
      interactionSession,
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
    ) &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'RESIZE_HANDLE'
      ? 1
      : 0
  },
  apply: (canvasState, interactionSession, customStrategyState) => {
    if (
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'RESIZE_HANDLE'
    ) {
      const edgePosition = interactionSession.activeControl.edgePosition
      const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
      if (interactionSession.interactionData.drag != null) {
        const drag = interactionSession.interactionData.drag
        const filteredSelectedElements = getDragTargets(selectedElements)
        const originalBoundingBox = getMultiselectBounds(
          canvasState.startingMetadata,
          filteredSelectedElements,
        )
        if (originalBoundingBox != null) {
          const lockedAspectRatio = getLockedAspectRatio(
            interactionSession,
            interactionSession.interactionData.modifiers,
            originalBoundingBox,
          )
          const centerBased = interactionSession.interactionData.modifiers.alt
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
            canvasState.startingMetadata,
            edgePosition,
            newBoundingBox,
            canvasState.scale,
            lockedAspectRatio,
            centerBased,
            canvasState.startingAllElementProps,
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
                canvasState.startingMetadata,
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
                  canvasState.startingMetadata,
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
                setSnappingGuidelines('mid-interaction', guidelinesWithSnappingVector), // TODO I think this will override the previous snapping guidelines
                pushIntendedBounds([{ target: selectedElement, frame: newFrame }]),
              ]
            },
          )
          return strategyApplicationResult([
            ...commandsForSelectedElements,
            updateHighlightedViews('mid-interaction', []),
            setCursorCommand('mid-interaction', pickCursorFromEdgePosition(edgePosition)),
            setElementsToRerenderCommand(selectedElements),
          ])
        }
      } else {
        return strategyApplicationResult([
          setCursorCommand('mid-interaction', pickCursorFromEdgePosition(edgePosition)),
          updateHighlightedViews('mid-interaction', []),
        ])
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
          'always',
          selectedElement,
          stylePropPathMappingFn(pin, ['style']),
          roundedDelta * pinDirection,
          horizontal ? elementParentBounds?.width : elementParentBounds?.height,
          true,
        )
      } else {
        const valueToSet = allPinsFromFrame(newFrame)[pin]
        return setCssLengthProperty(
          'always',
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

function getLockedAspectRatio(
  interactionData: InteractionSession,
  modifiers: Modifiers,
  originalBoundingBox: CanvasRectangle,
): number | null {
  if (interactionData.aspectRatioLock != null) {
    return interactionData.aspectRatioLock
  }
  if (modifiers.shift) {
    return originalBoundingBox.width / originalBoundingBox.height
  }
  return null
}
