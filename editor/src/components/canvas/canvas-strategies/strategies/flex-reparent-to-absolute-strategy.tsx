import { canvasPoint } from '../../../../core/shared/math-utils'
import type { EditorStatePatch } from '../../../editor/store/editor-state'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import type { CanvasStrategyFactory } from '../canvas-strategies'
import { pickCanvasStateFromEditorState } from '../canvas-strategies'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { baseAbsoluteReparentStrategy } from './absolute-reparent-strategy'
import { getEscapeHatchCommands } from './convert-to-absolute-and-move-strategy'
import { treatElementAsFragmentLike } from './fragment-like-helpers'
import { ifAllowedToReparent } from './reparent-helpers/reparent-helpers'
import type { ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
import { placeholderCloneCommands } from './reparent-utils'
import { flattenSelection } from './shared-move-strategies-helpers'

export function baseFlexReparentToAbsoluteStrategy(
  reparentTarget: ReparentTarget,
  fitness: number,
): CanvasStrategyFactory {
  return (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    customStrategyState: CustomStrategyState,
  ): CanvasStrategy | null => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

    return {
      id: `FLEX_REPARENT_TO_ABSOLUTE`,
      name: `Reparent (Abs)`,
      descriptiveLabel: 'Reparenting Flex Elements',
      icon: {
        category: 'modalities',
        type: 'reparent-large',
      },
      controlsToRender: [
        controlWithProps({
          control: ParentOutlines,
          props: { targetParent: reparentTarget.newParent.intendedParentPath },
          key: 'parent-outlines-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ParentBounds,
          props: { targetParent: reparentTarget.newParent.intendedParentPath },
          key: 'parent-bounds-control',
          show: 'visible-only-while-active',
        }),
        controlWithProps({
          control: ZeroSizedElementControls,
          props: { showAllPossibleElements: true },
          key: 'zero-size-control',
          show: 'visible-only-while-active',
        }),
      ],
      fitness: fitness,
      apply: (strategyLifecycle) => {
        const filteredSelectedElements = flattenSelection(selectedElements)
        const newParent = reparentTarget.newParent
        return ifAllowedToReparent(
          canvasState,
          canvasState.startingMetadata,
          filteredSelectedElements,
          newParent.intendedParentPath,
          () => {
            if (
              interactionSession == null ||
              interactionSession.interactionData.type !== 'DRAG' ||
              interactionSession.interactionData.drag == null
            ) {
              return emptyStrategyApplicationResult
            }

            // we want to keep a placeholder element where the original dragged element was to avoid the new parent shifting around on the screen
            const placeholderCommands = placeholderCloneCommands(
              canvasState,
              customStrategyState,
              filteredSelectedElements,
              newParent.intendedParentPath,
            )

            const escapeHatchCommands = getEscapeHatchCommands(
              filteredSelectedElements,
              canvasState.startingMetadata,
              canvasState,
              canvasPoint({ x: 0, y: 0 }),
              'dont-set-hugging-parent-to-fixed',
            ).commands

            return strategyApplicationResult(
              [
                ...placeholderCommands.commands,
                ...escapeHatchCommands,
                setElementsToRerenderCommand([
                  newParent.intendedParentPath,
                  ...filteredSelectedElements,
                ]),
                updateFunctionCommand(
                  'always',
                  (editorState, commandLifecycle): Array<EditorStatePatch> => {
                    const updatedCanvasState = pickCanvasStateFromEditorState(
                      editorState,
                      canvasState.builtInDependencies,
                    )
                    const absoluteReparentStrategyToUse = baseAbsoluteReparentStrategy(
                      reparentTarget,
                      0,
                      customStrategyState,
                    )
                    const reparentCommands =
                      absoluteReparentStrategyToUse(
                        updatedCanvasState,
                        interactionSession,
                        customStrategyState,
                      )?.apply(strategyLifecycle).commands ?? []

                    return foldAndApplyCommandsInner(
                      editorState,
                      [],
                      reparentCommands,
                      commandLifecycle,
                    ).statePatches
                  },
                ),
              ],
              {
                duplicatedElementNewUids: placeholderCommands.duplicatedElementNewUids,
              },
            )
          },
        )
      },
    }
  }
}
