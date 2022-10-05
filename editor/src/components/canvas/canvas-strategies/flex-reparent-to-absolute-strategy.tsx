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
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionLifecycle,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { getEscapeHatchCommands } from './convert-to-absolute-and-move-strategy'
import { InteractionSession, MissingBoundsHandling } from './interaction-state'
import { ifAllowedToReparent } from './reparent-helpers'
import {
  existingReparentSubjects,
  getFitnessForReparentStrategy,
  getReparentTargetUnified,
} from './reparent-strategy-helpers'
import { getDragTargets } from './shared-move-strategies-helpers'

function getFlexReparentToAbsoluteStrategy(
  id: 'FLEX_REPARENT_TO_ABSOLUTE' | 'FORCED_FLEX_REPARENT_TO_ABSOLUTE',
  name: string,
  missingBoundsHandling: MissingBoundsHandling,
): CanvasStrategy {
  return {
    id: id,
    name: () => name,
    isApplicable: (canvasState, interactionSession, metadata) => {
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
      controlWithProps({
        control: DragOutlineControl,
        props: {},
        key: 'ghost-outline-control',
        show: 'visible-only-while-active',
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
      // All 4 reparent strategies use the same fitness function getFitnessForReparentStrategy
      return getFitnessForReparentStrategy(
        'FLEX_REPARENT_TO_ABSOLUTE',
        canvasState,
        interactionSession,
        missingBoundsHandling,
      )
    },
    apply: (canvasState, interactionSession, customStrategyState, strategyLifecycle) => {
      const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
      const filteredSelectedElements = getDragTargets(selectedElements)
      return ifAllowedToReparent(
        canvasState,
        canvasState.startingMetadata,
        filteredSelectedElements,
        () => {
          if (
            interactionSession.interactionData.type !== 'DRAG' ||
            interactionSession.interactionData.drag == null
          ) {
            return emptyStrategyApplicationResult
          }

          const pointOnCanvas = offsetPoint(
            interactionSession.interactionData.originalDragStart,
            interactionSession.interactionData.drag,
          )

          const { newParent } = getReparentTargetUnified(
            existingReparentSubjects(filteredSelectedElements),
            pointOnCanvas,
            interactionSession.interactionData.modifiers.cmd,
            canvasState,
            canvasState.startingMetadata,
            canvasState.startingAllElementProps,
            missingBoundsHandling,
          )

          let duplicatedElementNewUids = {
            ...customStrategyState.duplicatedElementNewUids,
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
            canvasState.startingMetadata,
            canvasState,
            canvasPoint({ x: 0, y: 0 }),
          ).commands

          return strategyApplicationResult([
            ...placeholderCloneCommands,
            ...escapeHatchCommands,
            updateFunctionCommand(
              'always',
              (editorState, commandLifecycle): Array<EditorStatePatch> => {
                return runAbsoluteReparentStrategyForFreshlyConvertedElement(
                  canvasState.builtInDependencies,
                  editorState,
                  customStrategyState,
                  interactionSession,
                  commandLifecycle,
                  missingBoundsHandling,
                  strategyLifecycle,
                )
              },
            ),
          ])
        },
      )
    },
  }
}

function runAbsoluteReparentStrategyForFreshlyConvertedElement(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  customStrategyState: CustomStrategyState,
  interactionSession: InteractionSession,
  commandLifecycle: InteractionLifecycle,
  missingBoundsHandling: MissingBoundsHandling,
  strategyLifeCycle: InteractionLifecycle,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState, builtInDependencies)
  const isForced = missingBoundsHandling === 'allow-missing-bounds'

  const absoluteReparentStrategyToUse = isForced
    ? forcedAbsoluteReparentStrategy
    : absoluteReparentStrategy
  const reparentCommands = absoluteReparentStrategyToUse.apply(
    canvasState,
    interactionSession,
    customStrategyState,
    strategyLifeCycle,
  ).commands

  return foldAndApplyCommandsInner(editorState, [], [], reparentCommands, commandLifecycle)
    .statePatches
}

export const flexReparentToAbsoluteStrategy = getFlexReparentToAbsoluteStrategy(
  'FLEX_REPARENT_TO_ABSOLUTE',
  'Reparent (Abs)',
  'use-strict-bounds',
)
export const forcedFlexReparentToAbsoluteStrategy = getFlexReparentToAbsoluteStrategy(
  'FORCED_FLEX_REPARENT_TO_ABSOLUTE',
  'Reparent (Abs, Force)',
  'allow-missing-bounds',
)
