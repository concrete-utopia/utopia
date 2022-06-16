import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import { CanvasVector, offsetPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { AllElementProps, withUnderlyingTarget } from '../../editor/store/editor-state'
import { EdgePosition } from '../canvas-types'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { AbsoluteResizeControl } from '../controls/select-mode/absolute-resize-control'
import { ZeroSizeResizeControlWrapper } from '../controls/zero-sized-element-controls'
import { GuidelineWithSnappingVector } from '../guideline'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { getMultiselectBounds } from './shared-absolute-move-strategy-helpers'
import {
  pickCursorFromEdgePosition,
  resizeBoundingBox,
  runLegacyAbsoluteResizeSnapping,
} from './shared-absolute-resize-strategy-helpers'
import { createResizeCommands } from './shared-absolute-resize-strategy-helpers'

export const absoluteResizeDeltaStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_RESIZE_DELTA',
  name: 'Absolute Resize (Delta-based)',
  isApplicable: (canvasState, interactionState, metadata) => {
    if (
      canvasState.selectedElements.length === 1 &&
      !interactionState?.interactionData.modifiers.alt &&
      !interactionState?.interactionData.modifiers.shift // shift is aspect ratio locked resize implemented in absolute-resize-bounding-box-strategy.tsx
    ) {
      return canvasState.selectedElements.every((element) => {
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
    return absoluteResizeDeltaStrategy.isApplicable(
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
        const { snappedDragVector, guidelinesWithSnappingVector } = snapDrag(
          canvasState.selectedElements,
          sessionState.startingMetadata,
          drag,
          edgePosition,
          canvasState.scale,
          sessionState.startingAllElementProps,
        )

        const commandsForSelectedElements = canvasState.selectedElements.flatMap(
          (selectedElement) => {
            const element: JSXElement | null = withUnderlyingTarget(
              selectedElement,
              canvasState.projectContents,
              {},
              canvasState.openFile,
              null,
              (_, e) => e,
            )
            const elementParentBounds =
              MetadataUtils.findElementByElementPath(
                sessionState.startingMetadata, // TODO should this be using the current metadata?
                selectedElement,
              )?.specialSizeMeasurements.immediateParentBounds ?? null

            if (element == null) {
              return []
            }

            return createResizeCommands(
              element,
              selectedElement,
              edgePosition,
              snappedDragVector,
              elementParentBounds,
            )
          },
        )
        return {
          commands: [
            ...commandsForSelectedElements,
            updateHighlightedViews('transient', []),
            setCursorCommand('transient', pickCursorFromEdgePosition(edgePosition)),
            setSnappingGuidelines('transient', guidelinesWithSnappingVector),
            setElementsToRerenderCommand(canvasState.selectedElements),
          ],
          customState: null,
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

function snapDrag(
  selectedElements: Array<ElementPath>,
  startingMetadata: ElementInstanceMetadataMap,
  drag: CanvasVector,
  edgePosition: EdgePosition,
  canvasScale: number,
  allElementProps: AllElementProps,
): {
  snappedDragVector: CanvasVector
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVector>
} {
  const originalBoundingBox = getMultiselectBounds(startingMetadata, selectedElements)

  if (originalBoundingBox == null) {
    return { snappedDragVector: drag, guidelinesWithSnappingVector: [] }
  }

  const resizedUnsnappedBounds = resizeBoundingBox(
    originalBoundingBox,
    drag,
    edgePosition,
    null,
    'non-center-based',
  )
  const { snapDelta, guidelinesWithSnappingVector } = runLegacyAbsoluteResizeSnapping(
    selectedElements,
    startingMetadata,
    edgePosition,
    resizedUnsnappedBounds,
    canvasScale,
    null,
    'non-center-based',
    allElementProps,
  )
  const snappedDragVector = offsetPoint(drag, snapDelta)

  return {
    snappedDragVector: snappedDragVector,
    guidelinesWithSnappingVector: guidelinesWithSnappingVector,
  }
}
