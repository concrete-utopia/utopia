import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { EditorState, EditorStatePatch } from '../../../editor/store/editor-state'
import { CSSCursor } from '../../canvas-types'
import { foldAndApplyCommandsInner } from '../../commands/commands'
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
}

function runMoveStrategyForFreshlyDuplicatedElements(
  builtInDependencies: BuiltInDependencies,
  editorState: EditorState,
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

  const moveCommands =
    absoluteMoveStrategy(canvasState, interactionSession)?.apply(strategyLifecycle).commands ?? []

  return foldAndApplyCommandsInner(editorState, [], moveCommands, commandLifecycle).statePatches
}

function isApplicable(
  canvasState: InteractionCanvasState,
  filteredSelectedElements: ElementPath[],
) {
  return filteredSelectedElements.every((element) => {
    const elementMetadata = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      element,
    )

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
