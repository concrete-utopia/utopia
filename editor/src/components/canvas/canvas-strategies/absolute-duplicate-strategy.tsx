import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { CSSCursor } from '../canvas-types'
import { foldAndApplyCommandsInner } from '../commands/commands'
import { DuplicateElement, duplicateElement } from '../commands/duplicate-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateFunctionCommand } from '../commands/update-function-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import {
  pickCanvasStateFromEditorState,
  pickCanvasStateFromEditorStateWithMetadata,
} from './canvas-strategies'
import {
  CanvasStrategy,
  CustomStrategyState,
  getTargetPathsFromInteractionTarget,
  InteractionLifecycle,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'
import { getDragTargets } from './shared-move-strategies-helpers'

export const absoluteDuplicateStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_DUPLICATE',
  name: () => 'Duplicate',
  isApplicable: (canvasState, interactionSession, metadata) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (
      selectedElements.length > 0 &&
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.interactionData.modifiers.alt
    ) {
      const filteredSelectedElements = getDragTargets(selectedElements)
      return filteredSelectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        // for a multiselected elements, we only apply drag-to-duplicate if they are siblings
        // otherwise this would lead to an unpredictable behavior
        // we can revisit this once we have a more predictable reparenting
        const allDraggedElementsHaveTheSameParent = EP.pathsEqual(
          EP.parentPath(filteredSelectedElements[0]),
          EP.parentPath(element),
        )

        return (
          elementMetadata?.specialSizeMeasurements.position === 'absolute' &&
          allDraggedElementsHaveTheSameParent &&
          !EP.isRootElementOfInstance(element)
        )
      })
    }
    return false
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
  ],
  fitness: (canvasState, interactionSession) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (
      selectedElements.length > 0 &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'BOUNDING_AREA' &&
      interactionSession.interactionData.modifiers.alt
    ) {
      return 2
    }
    return 0
  },
  apply: (canvasState, interactionSession, customStrategyState, strategyLifecycle) => {
    const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    if (
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.interactionData.drag != null
    ) {
      const filteredSelectedElements = getDragTargets(selectedElements)

      let duplicatedElementNewUids = {
        ...customStrategyState.duplicatedElementNewUids,
      }
      let withDuplicatedMetadata: ElementInstanceMetadataMap = {
        ...canvasState.startingMetadata,
      }
      let duplicateCommands: Array<DuplicateElement> = []
      let newPaths: Array<ElementPath> = []

      filteredSelectedElements.forEach((selectedElement) => {
        const selectedElementString = EP.toString(selectedElement)
        const newUid =
          duplicatedElementNewUids[selectedElementString] ??
          generateUidWithExistingComponents(canvasState.projectContents)
        const newPath = EP.appendToPath(EP.parentPath(selectedElement), newUid)
        const newPathString = EP.toString(newPath)

        duplicatedElementNewUids[selectedElementString] = newUid
        withDuplicatedMetadata[newPathString] = {
          ...withDuplicatedMetadata[EP.toString(selectedElement)],
          elementPath: newPath,
        }

        duplicateCommands.push(duplicateElement('always', selectedElement, newUid))
        newPaths.push(newPath)
      })

      return strategyApplicationResult(
        [
          ...duplicateCommands,
          setElementsToRerenderCommand([...selectedElements, ...newPaths]),
          updateSelectedViews('always', newPaths),
          updateFunctionCommand('always', (editorState, commandLifecycle) =>
            runMoveStrategyForFreshlyDuplicatedElements(
              canvasState.builtInDependencies,
              editorState,
              customStrategyState,
              interactionSession,
              commandLifecycle,
              strategyLifecycle,
              withDuplicatedMetadata,
            ),
          ),
          setCursorCommand('mid-interaction', CSSCursor.Duplicate),
        ],
        {
          duplicatedElementNewUids: duplicatedElementNewUids,
        },
      )
    } else {
      // Fallback for when the checks above are not satisfied.
      return strategyApplicationResult([setCursorCommand('mid-interaction', CSSCursor.Duplicate)])
    }
  },
}

function runMoveStrategyForFreshlyDuplicatedElements(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
  customStrategyState: CustomStrategyState,
  interactionSession: InteractionSession,
  commandLifecycle: InteractionLifecycle,
  strategyLifecycle: InteractionLifecycle,
  metadata: ElementInstanceMetadataMap,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorStateWithMetadata(
    editorState,
    builtInDependencies,
    metadata,
  )

  const moveCommands = absoluteMoveStrategy.apply(
    canvasState,
    interactionSession,
    customStrategyState,
    strategyLifecycle,
  ).commands

  return foldAndApplyCommandsInner(editorState, [], [], moveCommands, commandLifecycle).statePatches
}
