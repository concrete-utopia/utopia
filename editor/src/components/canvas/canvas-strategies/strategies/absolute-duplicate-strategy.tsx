import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type {
  AllElementProps,
  DerivedState,
  EditorState,
  EditorStatePatch,
} from '../../../editor/store/editor-state'
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
  defaultCustomStrategyState,
  getTargetPathsFromInteractionTarget,
  strategyApplicationResult,
} from '../canvas-strategy-types'
import type { InteractionSession } from '../interaction-state'
import { flattenSelection } from './shared-move-strategies-helpers'
import { getElementFragmentLikeType } from './fragment-like-helpers'
import { setProperty } from '../../commands/set-property-command'
import * as PP from '../../../../core/shared/property-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { strictEvery } from '../../../../core/shared/array-utils'

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

  const result = isApplicable(flattenedSelectionForMultiSelect)

  if (result.type === 'not-applicable') {
    return null
  }

  const { commonParentPath } = result

  return {
    id: 'ABSOLUTE_DUPLICATE',
    name: 'Duplicate',
    descriptiveLabel: 'Duplicating Elements',
    icon: {
      category: 'modalities',
      type: 'duplicate',
    },
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

          if (
            !isElementSizelessDiv(
              canvasState.startingMetadata,
              canvasState.startingReconstructedDOMMetadata,
              canvasState.startingAllElementProps,
              canvasState.startingElementPathTree,
              selectedElement,
            )
          ) {
            duplicateCommands.push(
              setProperty('always', selectedElement, PP.create('style', 'position'), 'absolute'),
            )
          }
        })

        return strategyApplicationResult(
          [
            ...duplicateCommands,
            ...maybeAddContainLayoutCommand(commonParentPath),
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
    absoluteMoveStrategy(
      canvasState,
      interactionSession,
      { ...defaultCustomStrategyState(), action: 'duplicate' },
      'do-not-run-applicability-check',
    )?.strategy.apply(strategyLifecycle).commands ?? []

  return foldAndApplyCommandsInner(editorState, [], moveCommands, commandLifecycle).statePatches
}

type IsAbsoluteMoveApplicableResult =
  | {
      type: 'not-applicable'
    }
  | { type: 'applicable'; commonParentPath: ElementPath }

function isApplicable(filteredSelectedElements: ElementPath[]): IsAbsoluteMoveApplicableResult {
  const applicable = strictEvery(filteredSelectedElements, (element) => {
    // for a multiselected elements, we only apply drag-to-duplicate if they are siblings
    // otherwise this would lead to an unpredictable behavior
    // we can revisit this once we have a more predictable reparenting
    const allDraggedElementsHaveTheSameParent = EP.pathsEqual(
      EP.parentPath(filteredSelectedElements[0]),
      EP.parentPath(element),
    )

    return !EP.isRootElementOfInstance(element) && allDraggedElementsHaveTheSameParent
  })

  if (!applicable) {
    return { type: 'not-applicable' }
  }

  return { type: 'applicable', commonParentPath: EP.parentPath(filteredSelectedElements[0]) }
}

function isElementSizelessDiv(
  metadata: ElementInstanceMetadataMap,
  domReconstrucatedMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTrees: ElementPathTrees,
  elementPath: ElementPath,
): boolean {
  return (
    getElementFragmentLikeType(
      metadata,
      domReconstrucatedMetadata,
      allElementProps,
      elementPathTrees,
      elementPath,
    ) === 'sizeless-div'
  )
}

function maybeAddContainLayoutCommand(elementPath: ElementPath): CanvasCommand[] {
  return EP.isStoryboardPath(elementPath)
    ? []
    : [setProperty('always', elementPath, PP.create('style', 'contain'), 'layout')]
}
