import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import {
  appendToPath,
  isDescendantOf,
  parentPath,
  toString,
} from '../../../core/shared/element-path'
import { canvasPoint, offsetPoint, rectContainsPoint } from '../../../core/shared/math-utils'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { foldAndApplyCommandsInner, WhenToRun } from '../commands/commands'
import { duplicateElement } from '../commands/duplicate-element-command'
import { updateFunctionCommand } from '../commands/update-function-command'
import { wildcardPatch } from '../commands/wildcard-patch-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import {
  absoluteReparentStrategy,
  forcedAbsoluteReparentStrategy,
} from './absolute-reparent-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  strategyApplicationResult,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { getEscapeHatchCommands } from './convert-to-absolute-and-move-strategy'
import { InteractionSession, StrategyState } from './interaction-state'
import { ifAllowedToReparent } from './reparent-helpers'
import { findReparentStrategy, getReparentTargetUnified } from './reparent-strategy-helpers'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

function getFlexReparentToAbsoluteStrategy(
  id: 'FLEX_REPARENT_TO_ABSOLUTE' | 'FORCED_FLEX_REPARENT_TO_ABSOLUTE',
  name: string,
  allowMissingBounds: 'allow-missing-bounds' | 'use-strict-bounds',
  weight: number,
  allowFallback: boolean,
): CanvasStrategy {
  return {
    id: id,
    name: name,
    isApplicable: (canvasState, _interactionState, metadata) => {
      const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
      if (selectedElements.length == 1) {
        return MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
          selectedElements[0],
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
      // All 4 reparent strategies use the same fitness function findReparentStrategy
      const reparentStrategy = findReparentStrategy(
        canvasState,
        interactionState,
        strategyState,
        allowMissingBounds,
      ).strategy
      if (reparentStrategy === 'FLEX_REPARENT_TO_ABSOLUTE') {
        return weight
      } else if (reparentStrategy !== 'do-not-reparent' && allowFallback) {
        return weight - 1
      } else {
        return 0
      }
    },
    apply: (canvasState, interactionState, strategyState) => {
      const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
      const filteredSelectedElements = getDragTargets(selectedElements)
      return ifAllowedToReparent(canvasState, strategyState, filteredSelectedElements, () => {
        if (
          interactionState.interactionData.type !== 'DRAG' ||
          interactionState.interactionData.drag == null
        ) {
          return emptyStrategyApplicationResult
        }

        const pointOnCanvas = offsetPoint(
          interactionState.interactionData.originalDragStart,
          interactionState.interactionData.drag,
        )

        const { newParent } = getReparentTargetUnified(
          filteredSelectedElements,
          pointOnCanvas,
          interactionState.interactionData.modifiers.cmd,
          canvasState,
          strategyState.startingMetadata,
          strategyState.startingAllElementProps,
          allowMissingBounds,
        )

        let duplicatedElementNewUids = {
          ...strategyState.customStrategyState.duplicatedElementNewUids,
        }

        const placeholderCloneCommands = filteredSelectedElements.flatMap((element) => {
          const newParentADescendantOfCurrentParent =
            newParent != null && isDescendantOf(newParent, parentPath(element))

          if (newParentADescendantOfCurrentParent) {
            // if the new parent a descendant of the current parent, it means we want to keep a placeholder element where the original dragged element was, to avoid the new parent shifting around on the screen
            const selectedElementString = toString(element)
            const newUid =
              duplicatedElementNewUids[selectedElementString] ??
              generateUidWithExistingComponents(canvasState.projectContents)
            duplicatedElementNewUids[selectedElementString] = newUid

            const newPath = appendToPath(parentPath(element), newUid)

            return [
              duplicateElement('mid-interaction', element, newUid),
              wildcardPatch('mid-interaction', {
                hiddenInstances: { $push: [newPath] },
              }),
            ]
          } else {
            return []
          }
        })

        const escapeHatchCommands = getEscapeHatchCommands(
          filteredSelectedElements,
          strategyState.startingMetadata,
          canvasState,
          canvasPoint({ x: 0, y: 0 }),
        ).commands

        return strategyApplicationResult([
          ...placeholderCloneCommands,
          ...escapeHatchCommands,
          updateFunctionCommand('always', (editorState, lifecycle): Array<EditorStatePatch> => {
            return runAbsoluteReparentStrategyForFreshlyConvertedElement(
              canvasState.builtInDependencies,
              editorState,
              strategyState,
              interactionState,
              lifecycle,
              allowMissingBounds,
            )
          }),
        ])
      })
    },
  }
}

function runAbsoluteReparentStrategyForFreshlyConvertedElement(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  strategyState: StrategyState,
  interactionState: InteractionSession,
  commandLifecycle: 'mid-interaction' | 'end-interaction',
  allowMissingBounds: 'allow-missing-bounds' | 'use-strict-bounds',
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState, builtInDependencies)

  const absoluteReparentStrategyToUse =
    allowMissingBounds === 'allow-missing-bounds'
      ? forcedAbsoluteReparentStrategy
      : absoluteReparentStrategy
  const reparentCommands = absoluteReparentStrategyToUse.apply(
    canvasState,
    interactionState,
    strategyState,
  ).commands

  return foldAndApplyCommandsInner(editorState, [], [], reparentCommands, commandLifecycle)
    .statePatches
}

export const flexReparentToAbsoluteStrategy = getFlexReparentToAbsoluteStrategy(
  'FLEX_REPARENT_TO_ABSOLUTE',
  'Reparent (Abs)',
  'use-strict-bounds',
  3,
  true,
)
export const forcedFlexReparentToAbsoluteStrategy = getFlexReparentToAbsoluteStrategy(
  'FORCED_FLEX_REPARENT_TO_ABSOLUTE',
  'Reparent (Abs, Force)',
  'allow-missing-bounds',
  0.5,
  false,
)
