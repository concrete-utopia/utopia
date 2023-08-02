import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { CSSCursor } from '../../canvas-types'
import type { CanvasCommand } from '../../commands/commands'
import { foldAndApplyCommandsInner } from '../../commands/commands'
import { duplicateElement } from '../../commands/duplicate-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import type {
  CanvasStrategy,
  CustomStrategyState,
  InteractionCanvasState,
  InteractionLifecycle,
} from '../canvas-strategy-types'
import {
  controlWithProps,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { flattenSelection } from './shared-move-strategies-helpers'
import { replaceFragmentLikePathsWithTheirChildrenRecursive } from './fragment-like-helpers'

export function absoluteDuplicateStrategy(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
): CanvasStrategy | null {
  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (
    selectedElements.length === 0 ||
    interactionSession == null ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.activeControl.type !== 'BOUNDING_AREA' ||
    !interactionSession.interactionData.modifiers.alt
  ) {
    return null
  }

  const isDragging = interactionSession.interactionData.drag != null
  const flattenedSelectionForMultiSelect = flattenSelection(selectedElements)

  if (!isApplicable(canvasState, flattenedSelectionForMultiSelect)) {
    return null
  }

  return {
    id: 'ABSOLUTE_DUPLICATE',
    name: 'Duplicate',
    controlsToRender: [
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: flattenedSelectionForMultiSelect },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: flattenedSelectionForMultiSelect },
        key: 'parent-bounds-control',
        show: 'visible-only-while-active',
      }),
    ],
    fitness: 2,
    apply: (strategyLifecycle) => {
      if (isDragging) {
        let duplicatedElementNewUids = {
          ...customStrategyState.duplicatedElementNewUids,
        }
        let duplicateCommands: Array<CanvasCommand> = []
        let newPaths: Array<ElementPath> = []

        flattenedSelectionForMultiSelect.forEach((selectedElement) => {
          const selectedElementString = EP.toString(selectedElement)
          const newUid =
            duplicatedElementNewUids[selectedElementString] ??
            generateUidWithExistingComponents(canvasState.projectContents)
          duplicatedElementNewUids[selectedElementString] = newUid

          const newPath = EP.appendToPath(EP.parentPath(selectedElement), newUid)

          newPaths.push(newPath)
          duplicateCommands.push(duplicateElement('always', selectedElement, newUid, 'before'))
        })

        return strategyApplicationResult(
          [
            ...duplicateCommands,
            setElementsToRerenderCommand([...selectedElements, ...newPaths]),
            updateSelectedViews('always', selectedElements),
            updateFunctionCommand('always', (editorState, commandLifecycle) =>
              runMoveStrategy(
                canvasState,
                editorState,
                interactionSession,
                commandLifecycle,
                strategyLifecycle,
              ),
            ),
            setCursorCommand(CSSCursor.Duplicate),
          ],
          {
            duplicatedElementNewUids: duplicatedElementNewUids,
          },
        )
      } else {
        // Fallback for when the checks above are not satisfied.
        return strategyApplicationResult([setCursorCommand(CSSCursor.Duplicate)])
      }
    },
  }
}

function runMoveStrategy(
  canvasState: InteractionCanvasState,
  editorState: EditorState,
  interactionSession: InteractionSession,
  commandLifecycle: InteractionLifecycle,
  strategyLifecycle: InteractionLifecycle,
): Array<EditorStatePatch> {
  const moveCommands =
    absoluteMoveStrategy(canvasState, interactionSession)?.strategy.apply(strategyLifecycle)
      .commands ?? []

  return foldAndApplyCommandsInner(editorState, [], moveCommands, commandLifecycle).statePatches
}

function isApplicable(
  canvasState: InteractionCanvasState,
  filteredSelectedElements: ElementPath[],
) {
  return filteredSelectedElements.every((element) => {
    // for a multiselected elements, we only apply drag-to-duplicate if they are siblings
    // otherwise this would lead to an unpredictable behavior
    // we can revisit this once we have a more predictable reparenting
    const allDraggedElementsHaveTheSameParent = EP.pathsEqual(
      EP.parentPath(filteredSelectedElements[0]),
      EP.parentPath(element),
    )

    const unrolledChildren = replaceFragmentLikePathsWithTheirChildrenRecursive(
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
      canvasState.startingElementPathTree,
      [element],
    )

    const isElementAbsolute = unrolledChildren.every((path) =>
      MetadataUtils.isPositionAbsolute(
        MetadataUtils.findElementByElementPath(canvasState.startingMetadata, path),
      ),
    )

    return (
      !EP.isRootElementOfInstance(element) &&
      allDraggedElementsHaveTheSameParent &&
      isElementAbsolute
    )
  })
}
