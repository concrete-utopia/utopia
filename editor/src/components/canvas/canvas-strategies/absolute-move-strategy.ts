import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasPoint, offsetPoint, zeroCanvasPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { runLegacyAbsoluteMoveSnapping } from '../controls/guideline-helpers'
import { determineConstrainedDragAxis } from '../controls/select-mode/move-utils'
import { ConstrainedDragAxis, GuidelineWithSnappingVector } from '../guideline'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
} from './canvas-strategy-types'
import { DragInteractionData, StrategyState } from './interaction-state'
import {
  getAbsoluteMoveCommandsForSelectedElement,
  getDragTargets,
  getMultiselectBounds,
} from './shared-absolute-move-strategy-helpers'

export const absoluteMoveStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_MOVE',
  name: 'Absolute Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
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
  controlsToRender: [], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return absoluteMoveStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
      const { snappedDragVector, guidelinesWithSnappingVector } = snapDragAbsoluteMove(
        canvasState,
        interactionState.interactionData,
        sessionState,
      )
      const commandsForSelectedElements = filteredSelectedElements.flatMap((selectedElement) =>
        getAbsoluteMoveCommandsForSelectedElement(
          selectedElement,
          snappedDragVector,
          canvasState,
          sessionState,
        ),
      )
      const transientMoveCommands = getTransientMoveCommands(guidelinesWithSnappingVector)
      return {
        commands: [...commandsForSelectedElements, ...transientMoveCommands],
        customState: null,
      }
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

export function snapDragAbsoluteMove(
  canvasState: InteractionCanvasState,
  interactionData: DragInteractionData,
  strategyState: StrategyState,
): {
  snappedDragVector: CanvasPoint
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVector>
} {
  const drag = interactionData.drag
  if (drag == null) {
    return { snappedDragVector: zeroCanvasPoint, guidelinesWithSnappingVector: [] }
  } else {
    const shiftKeyPressed = interactionData.modifiers.shift
    const constrainedDragAxis = shiftKeyPressed ? determineConstrainedDragAxis(drag) : null
    const multiselectBounds = getMultiselectBounds(
      strategyState.startingMetadata,
      canvasState.selectedElements,
    )

    // This is the entry point to extend the list of snapping strategies, if we want to add more

    const { snappedDragVector, guidelinesWithSnappingVector } = runLegacyAbsoluteMoveSnapping(
      drag,
      constrainedDragAxis,
      strategyState.startingMetadata,
      canvasState.selectedElements,
      canvasState.scale,
      multiselectBounds,
    )

    return { snappedDragVector, guidelinesWithSnappingVector }
  }
}

export function getTransientMoveCommands(
  guidelinesWithSnappingVector: Array<GuidelineWithSnappingVector>,
) {
  return [
    updateHighlightedViews('transient', []),
    setSnappingGuidelines('transient', guidelinesWithSnappingVector),
  ]
}
