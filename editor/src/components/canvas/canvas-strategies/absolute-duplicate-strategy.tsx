import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorState, EditorStatePatch } from '../../editor/store/editor-state'
import { foldAndApplyCommandsInner, TransientOrNot } from '../commands/commands'
import { DuplicateElement, duplicateElement } from '../commands/duplicate-element-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateFunctionCommand } from '../commands/update-function-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { pickCanvasStateFromEditorState } from './canvas-strategies'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { InteractionSession, interactionSession, StrategyState } from './interaction-state'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

export const absoluteDuplicateStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_DUPLICATE',
  name: 'Duplicate Absolute Elements',
  isApplicable: (canvasState, interactionState, metadata) => {
    if (
      canvasState.selectedElements.length > 0 &&
      interactionState != null &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.modifiers.alt
    ) {
      const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
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
          allDraggedElementsHaveTheSameParent
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
  fitness: (canvasState, interactionState) => {
    if (
      canvasState.selectedElements.length > 0 &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.activeControl.type === 'BOUNDING_AREA' &&
      interactionState.interactionData.modifiers.alt &&
      interactionState.interactionData.drag != null
    ) {
      return 2
    }
    return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.drag != null
    ) {
      const { selectedElements } = canvasState
      const filteredSelectedElements = getDragTargets(selectedElements)

      let duplicatedElementNewUids = {
        ...strategyState.customStrategyState.duplicatedElementNewUids,
      }
      let withDuplicatedMetadata: ElementInstanceMetadataMap = { ...strategyState.startingMetadata }
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

        duplicateCommands.push(duplicateElement('permanent', selectedElement, newUid))
        newPaths.push(newPath)
      })

      return {
        commands: [
          ...duplicateCommands,
          setElementsToRerenderCommand([...canvasState.selectedElements, ...newPaths]),
          updateSelectedViews('permanent', newPaths),
          updateFunctionCommand('permanent', (editorState, transient) =>
            runMoveStrategyForFreshlyDuplicatedElements(
              editorState,
              {
                ...strategyState,
                startingMetadata: withDuplicatedMetadata,
              },
              interactionState,
              transient,
            ),
          ),
        ],
        customState: {
          ...strategyState.customStrategyState,
          duplicatedElementNewUids: duplicatedElementNewUids,
        },
      }
    }
    // Fallback for when the checks above are not satisfied.
    return emptyStrategyApplicationResult
  },
}

function runMoveStrategyForFreshlyDuplicatedElements(
  editorState: EditorState,
  strategyState: StrategyState,
  interactionState: InteractionSession,
  transient: TransientOrNot,
): Array<EditorStatePatch> {
  const canvasState = pickCanvasStateFromEditorState(editorState)

  const moveCommands = absoluteMoveStrategy.apply(
    canvasState,
    interactionState,
    strategyState,
  ).commands

  return foldAndApplyCommandsInner(editorState, [], [], moveCommands, transient).statePatches
}
