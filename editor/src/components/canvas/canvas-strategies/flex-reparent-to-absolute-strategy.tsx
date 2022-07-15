import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { canvasPoint, offsetPoint, rectContainsPoint } from '../../../core/shared/math-utils'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { foldAndApplyCommandsInner, TransientOrNot } from '../commands/commands'
import { updateFunctionCommand } from '../commands/update-function-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { getEscapeHatchCommands } from './escape-hatch-strategy'
import { InteractionSession, StrategyState } from './interaction-state'

export const flexReparentToAbsoluteStrategy: CanvasStrategy = {
  id: 'FLEX_REPARENT_TO_ABSOLUTE',
  name: 'Flex Reparent to Absolute',
  isApplicable: (canvasState, _interactionState, metadata) => {
    if (canvasState.selectedElements.length == 1) {
      return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        canvasState.selectedElements[0],
        metadata,
      )
    } else {
      return false
    }
  },
  controlsToRender: [
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
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
  ],
  fitness: (canvasState, interactionState, strategyState) => {
    if (
      flexReparentToAbsoluteStrategy.isApplicable(
        canvasState,
        interactionState,
        strategyState.startingMetadata,
        strategyState.startingAllElementProps,
      ) &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA' &&
      interactionState.interactionData.drag != null
    ) {
      const pointOnCanvas = offsetPoint(
        interactionState.interactionData.dragStart,
        interactionState.interactionData.drag,
      )

      const target = canvasState.selectedElements[0]

      const parentElementBounds = MetadataUtils.getParent(
        strategyState.startingMetadata,
        target,
      )?.globalFrame

      const isPointInParentBounds =
        parentElementBounds != null && rectContainsPoint(parentElementBounds, pointOnCanvas)

      if (!isPointInParentBounds) {
        // todo only return 2 if the target parent is absolute, create new FLEX_TO_FLEX_REPARENT strategy
        return 2 // 2 here to beat flexReorderStrategy
      }
    }
    return 0 // fix fallback return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (
      interactionState.interactionData.type !== 'DRAG' ||
      interactionState.interactionData.drag == null
    ) {
      return emptyStrategyApplicationResult
    }

    const escapeHatchCommands = getEscapeHatchCommands(
      canvasState.selectedElements,
      strategyState.startingMetadata,
      canvasState,
      canvasPoint({ x: 0, y: 0 }),
    ).commands

    return {
      commands: [
        ...escapeHatchCommands,
        updateFunctionCommand('permanent', (editorState, transient): Array<EditorStatePatch> => {
          return runAbsoluteReparentStrategyForFreshlyConvertedElement(
            editorState,
            strategyState,
            interactionState,
            transient,
          )
        }),
      ],
      customState: null,
    }
  },
}

function runAbsoluteReparentStrategyForFreshlyConvertedElement(
  editorState: EditorState,
  strategyState: StrategyState,
  interactionState: InteractionSession,
  transient: TransientOrNot,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState)

  const reparentCommands = absoluteReparentStrategy.apply(
    canvasState,
    interactionState,
    strategyState,
  ).commands

  return foldAndApplyCommandsInner(editorState, [], [], reparentCommands, transient).statePatches
}
