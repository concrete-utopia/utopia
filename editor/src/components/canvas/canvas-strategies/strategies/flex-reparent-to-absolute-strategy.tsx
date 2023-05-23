import { canvasPoint } from '../../../../core/shared/math-utils'
import { EditorStatePatch } from '../../../editor/store/editor-state'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { ParentBounds } from '../../controls/parent-bounds'
import { ParentOutlines } from '../../controls/parent-outlines'
import { ZeroSizedElementControls } from '../../controls/zero-sized-element-controls'
import { CanvasStrategyFactory, pickCanvasStateFromEditorState } from '../canvas-strategies'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { baseAbsoluteReparentStrategy } from './absolute-reparent-strategy'
import { getEscapeHatchCommands } from './convert-to-absolute-and-move-strategy'
import { treatElementAsContentAffecting } from './group-like-helpers'
import { ifAllowedToReparent } from './reparent-helpers/reparent-helpers'
import { ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
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
        return ifAllowedToReparent(
          canvasState,
          canvasState.startingMetadata,
          filteredSelectedElements,
          () => {
            if (
              interactionSession == null ||
              interactionSession.interactionData.type !== 'DRAG' ||
              interactionSession.interactionData.drag == null
            ) {
              return emptyStrategyApplicationResult
            }

            const newParent = reparentTarget.newParent

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
            ).commands

            return strategyApplicationResult(
              [
                ...placeholderCommands.commands,
                ...escapeHatchCommands,
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
