import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { CanvasPoint } from '../../../core/shared/math-utils'
import { CanvasFrameAndTarget, CSSCursor } from '../canvas-types'
import { AdjustCssLengthProperty } from '../commands/adjust-css-length-command'
import { CanvasCommand } from '../commands/commands'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { pushIntendedBounds } from '../commands/push-intended-bounds-command'
import { setSnappingGuidelines } from '../commands/set-snapping-guidelines-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { determineConstrainedDragAxis } from '../controls/select-mode/move-utils'
import {
  CanvasStrategy,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'
import {
  getAbsoluteMoveCommandsForSelectedElement,
  getDragTargets,
  snapDrag,
} from './shared-absolute-move-strategy-helpers'
import { honoursPropsPosition } from './absolute-utils'
import { collectParentAndSiblingGuidelines } from '../controls/guideline-helpers'

export const absoluteMoveStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_MOVE',
  name: () => 'Move',
  isApplicable: (canvasState, _interactionState, metadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (selectedElements.length > 0) {
      const filteredSelectedElements = getDragTargets(selectedElements)
      return filteredSelectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)
        return (
          elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
          honoursPropsPosition(canvasState, element)
        )
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
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
    ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA'
      ? 1
      : 0
  },
  apply: (canvasState, interactionState, customStrategyState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      const getAdjustMoveCommands = (
        snappedDragVector: CanvasPoint,
      ): {
        commands: Array<AdjustCssLengthProperty>
        intendedBounds: Array<CanvasFrameAndTarget>
      } => {
        const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
        const filteredSelectedElements = getDragTargets(selectedElements)
        let commands: Array<AdjustCssLengthProperty> = []
        let intendedBounds: Array<CanvasFrameAndTarget> = []
        filteredSelectedElements.forEach((selectedElement) => {
          const elementResult = getAbsoluteMoveCommandsForSelectedElement(
            selectedElement,
            snappedDragVector,
            canvasState,
            interactionState,
          )
          commands.push(...elementResult.commands)
          intendedBounds.push(...elementResult.intendedBounds)
        })
        return { commands, intendedBounds }
      }
      return applyAbsoluteMoveCommon(
        canvasState,
        interactionState,
        customStrategyState,
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
  customStrategyState: CustomStrategyState,
  getMoveCommands: (snappedDragVector: CanvasPoint) => {
    commands: Array<CanvasCommand>
    intendedBounds: Array<CanvasFrameAndTarget>
  },
): StrategyApplicationResult {
  if (
    interactionState.interactionData.type === 'DRAG' &&
    interactionState.interactionData.drag != null
  ) {
    const drag = interactionState.interactionData.drag
    const shiftKeyPressed = interactionState.interactionData.modifiers.shift
    const cmdKeyPressed = interactionState.interactionData.modifiers.cmd
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (cmdKeyPressed) {
      const commandsForSelectedElements = getMoveCommands(drag)

      return strategyApplicationResult([
        ...commandsForSelectedElements.commands,
        pushIntendedBounds(commandsForSelectedElements.intendedBounds),
        updateHighlightedViews('mid-interaction', []),
        setElementsToRerenderCommand(selectedElements),
        setCursorCommand('mid-interaction', CSSCursor.Select),
      ])
    } else {
      const constrainedDragAxis =
        shiftKeyPressed && drag != null ? determineConstrainedDragAxis(drag) : null

      const targetsForSnapping = selectedElements.map(
        (path) => interactionState.updatedTargetPaths[EP.toString(path)] ?? path,
      )
      const moveGuidelines = collectParentAndSiblingGuidelines(
        canvasState.startingMetadata,
        targetsForSnapping,
      )

      const { snappedDragVector, guidelinesWithSnappingVector } = snapDrag(
        drag,
        constrainedDragAxis,
        canvasState.startingMetadata,
        selectedElements,
        moveGuidelines,
        canvasState.scale,
      )
      const commandsForSelectedElements = getMoveCommands(snappedDragVector)
      return strategyApplicationResult([
        ...commandsForSelectedElements.commands,
        updateHighlightedViews('mid-interaction', []),
        setSnappingGuidelines('mid-interaction', guidelinesWithSnappingVector),
        pushIntendedBounds(commandsForSelectedElements.intendedBounds),
        setElementsToRerenderCommand([...selectedElements, ...targetsForSnapping]),
        setCursorCommand('mid-interaction', CSSCursor.Select),
      ])
    }
  } else {
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  }
}
