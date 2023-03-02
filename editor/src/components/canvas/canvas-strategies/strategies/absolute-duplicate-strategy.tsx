import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { CSSCursor } from '../../canvas-types'
import { CanvasCommand, foldAndApplyCommandsInner } from '../../commands/commands'
import { DuplicateElement, duplicateElement } from '../../commands/duplicate-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../../commands/set-elements-to-rerender-command'
import { updateFunctionCommand } from '../../commands/update-function-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import { ImmediateParentBounds } from '../../controls/parent-bounds'
import { ImmediateParentOutlines } from '../../controls/parent-outlines'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { pickCanvasStateFromEditorStateWithMetadata } from '../canvas-strategies'
import {
  CanvasStrategy,
  controlWithProps,
  CustomStrategyState,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  InteractionLifecycle,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'
import { getDragTargets } from './shared-move-strategies-helpers'
import { treatElementAsContentAffecting } from './group-like-helpers'
import { updatePropIfExists } from '../../commands/update-prop-if-exists-command'
import { create } from '../../../../core/shared/property-path'

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
  const filteredSelectedElements = getDragTargets(selectedElements)

  if (!isApplicable(canvasState, filteredSelectedElements)) {
    return null
  }

  return {
    id: 'ABSOLUTE_DUPLICATE',
    name: 'Duplicate',
    controlsToRender: [
      controlWithProps({
        control: ImmediateParentOutlines,
        props: { targets: filteredSelectedElements },
        key: 'parent-outlines-control',
        show: 'visible-only-while-active',
      }),
      controlWithProps({
        control: ImmediateParentBounds,
        props: { targets: filteredSelectedElements },
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

        filteredSelectedElements.forEach((selectedElement) => {
          const selectedElementString = EP.toString(selectedElement)
          const oldUid = EP.toUid(selectedElement)
          const newUid =
            duplicatedElementNewUids[selectedElementString] ??
            generateUidWithExistingComponents(canvasState.projectContents)
          duplicatedElementNewUids[selectedElementString] = newUid

          const newPath = EP.appendToPath(EP.parentPath(selectedElement), newUid)

          newPaths.push(newPath)
          duplicateCommands.push(
            duplicateElement('always', selectedElement, newUid, 'before'),
            /**
             * TODO: the UIDs are swapped to make it seem like that the element being dragged
             * (the original element) is the new, duplicated element. While this works on the canvas,
             * in the code editor, the wrong code snippet is highlighted until the affected files are saved.
             *
             * I added this as a fallback solution, definitely needs a better approach
             */
            updatePropIfExists('on-complete', selectedElement, create('data-uid'), newUid),
            updatePropIfExists('on-complete', newPath, create('data-uid'), oldUid),
          )
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

    const isElementContentAffecting = treatElementAsContentAffecting(
      canvasState.startingMetadata,
      canvasState.startingAllElementProps,
      element,
    )

    const isElementAbsolute = isElementContentAffecting
      ? MetadataUtils.getChildrenPathsUnordered(canvasState.startingMetadata, element).every(
          (path) =>
            MetadataUtils.isPositionAbsolute(
              MetadataUtils.findElementByElementPath(canvasState.startingMetadata, path),
            ),
        )
      : MetadataUtils.isPositionAbsolute(
          MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
        )

    return (
      !EP.isRootElementOfInstance(element) &&
      allDraggedElementsHaveTheSameParent &&
      isElementAbsolute
    )
  })
}
