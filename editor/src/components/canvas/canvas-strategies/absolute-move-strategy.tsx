import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { CanvasPoint, offsetPoint, zeroCanvasPoint } from '../../../core/shared/math-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CSSCursor } from '../canvas-types'
import { CanvasCommand } from '../commands/commands'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { runLegacyAbsoluteMoveSnapping } from '../controls/guideline-helpers'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { determineConstrainedDragAxis } from '../controls/select-mode/move-utils'
import { ConstrainedDragAxis, GuidelineWithSnappingVector } from '../guideline'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { DragInteractionData, InteractionSession, StrategyState } from './interaction-state'
import {
  getAbsoluteMoveCommandsForSelectedElement,
  getDragTargets,
  getMultiselectBounds,
  snapDrag,
} from './shared-absolute-move-strategy-helpers'

export const absoluteMoveStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_MOVE',
  name: 'Absolute Move (Delta-based)',
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
  controlsToRender: [
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
  ], // Uses existing hooks in select-mode-hooks.tsx
  fitness: (canvasState, interactionState, sessionState) => {
    return absoluteMoveStrategy.isApplicable(
      canvasState,
      interactionState,
      sessionState.startingMetadata,
      sessionState.startingAllElementProps,
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
      const getAdjustMoveCommands = (snappedDragVector: CanvasPoint): Array<CanvasCommand> => {
        const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
        return filteredSelectedElements.flatMap((selectedElement) =>
          getAbsoluteMoveCommandsForSelectedElement(
            selectedElement,
            snappedDragVector,
            canvasState,
            sessionState,
          ),
        )
      }
      return applyAbsoluteMoveCommon(
        canvasState,
        interactionState,
        sessionState,
        getAdjustMoveCommands,
      )
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

export function applyAbsoluteMoveCommon(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyState,
  getMoveCommands: (snappedDragVector: CanvasPoint) => Array<CanvasCommand>,
): StrategyApplicationResult {
  if (
    interactionState.interactionData.type === 'DRAG' &&
    interactionState.interactionData.drag != null
  ) {
    const drag = interactionState.interactionData.drag
    const shiftKeyPressed = interactionState.interactionData.modifiers.shift
    const cmdKeyPressed = interactionState.interactionData.modifiers.cmd
    if (cmdKeyPressed) {
      const commandsForSelectedElements = getMoveCommands(drag)
      return {
        commands: [
          ...commandsForSelectedElements,
          updateHighlightedViews('transient', []),
          setElementsToRerenderCommand(canvasState.selectedElements),
          setCursorCommand('transient', CSSCursor.Select),
        ],
        customState: null,
      }
    } else {
      const constrainedDragAxis =
        shiftKeyPressed && drag != null ? determineConstrainedDragAxis(drag) : null
      const { snappedDragVector, guidelinesWithSnappingVector } = snapDrag(
        drag,
        constrainedDragAxis,
        strategyState.startingMetadata,
        canvasState.selectedElements,
        canvasState.scale,
      )
      const commandsForSelectedElements = getMoveCommands(snappedDragVector)
      return {
        commands: [
          ...commandsForSelectedElements,
          updateHighlightedViews('transient', []),
          setSnappingGuidelines('transient', guidelinesWithSnappingVector),
          setElementsToRerenderCommand(canvasState.selectedElements),
          setCursorCommand('transient', CSSCursor.Select),
        ],
        customState: null,
      }
    }
  } else {
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  }
}
