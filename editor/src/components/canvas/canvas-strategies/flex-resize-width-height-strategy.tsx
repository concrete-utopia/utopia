import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap, JSXElement } from '../../../core/shared/element-template'
import { CanvasVector, offsetPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { withUnderlyingTarget } from '../../editor/store/editor-state'
import { EdgePosition } from '../canvas-types'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { GuidelineWithSnappingVector } from '../guideline'
import { CanvasStrategy } from './canvas-strategy-types'

import { getMultiselectBounds } from './shared-absolute-move-strategy-helpers'
import {
  resizeBoundingBox,
  runLegacyAbsoluteResizeSnapping,
  createResizeCommands,
} from './shared-absolute-resize-strategy-helpers'
import { AbsoluteResizeControl } from '../controls/select-mode/absolute-resize-control'

export const flexResizeWidthStrategy: CanvasStrategy = {
  id: 'FLEX_RESIZE_WIDTH_HEIGHT',
  name: 'Flex Resize (Width/Height)',
  isApplicable: (canvasState, interactionState, metadata) => {
    if (
      canvasState.selectedElements.length === 1 &&
      !interactionState?.interactionData.modifiers.alt &&
      !interactionState?.interactionData.modifiers.shift // shift is aspect ratio locked resize implemented in absolute-resize-bounding-box-strategy.tsx
    ) {
      return canvasState.selectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return elementMetadata?.specialSizeMeasurements.position !== 'absolute'
      })
    } else {
      return false
    }
  },
  controlsToRender: [
    { control: AbsoluteResizeControl, key: 'flex-resize-control', show: 'always-visible' },
  ],
  fitness: (canvasState, interactionState, sessionState) => {
    return flexResizeWidthStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null &&
      interactionState.activeControl.type === 'RESIZE_HANDLE'
    ) {
      const drag = interactionState.interactionData.drag
      const edgePosition = interactionState.activeControl.edgePosition
      const { snappedDragVector, guidelinesWithSnappingVector } = snapDrag(
        canvasState.selectedElements,
        sessionState.startingMetadata,
        drag,
        edgePosition,
        canvasState.scale,
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
      return [
        ...commandsForSelectedElements,
        updateHighlightedViews('transient', []),
        setSnappingGuidelines('transient', guidelinesWithSnappingVector),
      ]
    }
    // Fallback for when the checks above are not satisfied.
    return []
  },
}

function snapDrag(
  selectedElements: Array<ElementPath>,
  startingMetadata: ElementInstanceMetadataMap,
  drag: CanvasVector,
  edgePosition: EdgePosition,
  canvasScale: number,
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
  )
  const snappedDragVector = offsetPoint(drag, snapDelta)

  return {
    snappedDragVector: snappedDragVector,
    guidelinesWithSnappingVector: guidelinesWithSnappingVector,
  }
}
