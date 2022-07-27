import * as EP from '../../../core/shared/element-path'
import * as PP from '../../../core/shared/property-path'
import { AllElementProps } from '../../editor/store/editor-state'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'
import { getReparentTarget } from '../canvas-utils'
import { ElementPath, PropertyPath } from '../../../core/shared/project-file-types'
import { reparentElement } from '../commands/reparent-element-command'
import { getReorderIndex } from './flex-reorder-strategy'
import { offsetPoint } from '../../../core/shared/math-utils'
import { reorderElement } from '../commands/reorder-element-command'
import { CSSCursor } from '../canvas-types'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateHighlightedViews } from '../commands/update-highlighted-views-command'
import { CanvasCommand, foldAndApplyCommandsInner } from '../commands/commands'
import { deleteProperties } from '../commands/delete-properties-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { findReparentStrategy, getReparentTargetForFlexElement } from './reparent-strategy-helpers'
import { getReparentCommands } from './reparent-utils'
import { ifAllowedToReparent } from './reparent-helpers'

const propertiesToRemove: Array<PropertyPath> = [
  PP.create(['style', 'position']),
  PP.create(['style', 'left']),
  PP.create(['style', 'top']),
  PP.create(['style', 'right']),
  PP.create(['style', 'bottom']),
]

export const absoluteReparentToFlexStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_REPARENT_TO_FLEX',
  name: 'Absolute Reparent to Flex',
  isApplicable: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
  ): boolean {
    if (
      canvasState.selectedElements.length === 1 &&
      interactionSession != null &&
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.interactionData.modifiers.cmd
    ) {
      const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
      if (filteredSelectedElements.length === 1) {
        const elementMetadata = MetadataUtils.findElementByElementPath(
          metadata,
          filteredSelectedElements[0],
        )

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
      }
    }
    return false
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
  fitness: function (
    canvasState: InteractionCanvasState,
    interactionState: InteractionSession,
    strategyState: StrategyState,
  ): number {
    // All 4 reparent strategies use the same fitness function findReparentStrategy
    const reparentStrategy = findReparentStrategy(
      canvasState,
      interactionState,
      strategyState,
    ).strategy
    if (reparentStrategy === 'ABSOLUTE_REPARENT_TO_FLEX') {
      return 3
    }
    return 0
  },
  apply: function (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession,
    strategyState: StrategyState,
  ): StrategyApplicationResult {
    const filteredSelectedElements = getDragTargets(canvasState.selectedElements)

    return ifAllowedToReparent(canvasState, filteredSelectedElements, () => {
      if (
        interactionSession.interactionData.type == 'DRAG' &&
        interactionSession.interactionData.drag != null
      ) {
        const reparentResult = getReparentTargetForFlexElement(
          filteredSelectedElements,
          interactionSession,
          canvasState,
          strategyState,
        )

        if (
          reparentResult.shouldReparent &&
          reparentResult.newParent != null &&
          filteredSelectedElements.length === 1
        ) {
          const target = filteredSelectedElements[0]
          const newParent = reparentResult.newParent
          // Reparent the element.
          const newPath = EP.appendToPath(reparentResult.newParent, EP.toUid(target))
          const reparentCommands = getReparentCommands(
            canvasState.projectContents,
            canvasState.nodeModules,
            canvasState.openFile,
            target,
            reparentResult.newParent,
          )

          // Strip the `position`, positional and dimension properties.
          const commandToRemoveProperties = deleteProperties(
            'permanent',
            newPath,
            propertiesToRemove,
          )

          const commandsBeforeReorder = [
            ...reparentCommands,
            updateSelectedViews('permanent', [newPath]),
          ]

          const commandsAfterReorder = [
            commandToRemoveProperties,
            setElementsToRerenderCommand([newPath]),
            updateHighlightedViews('transient', []),
            setCursorCommand('transient', CSSCursor.Move),
          ]

          let commands: Array<CanvasCommand>
          if (reparentResult.shouldReorder) {
            // Reorder the newly reparented element into the flex ordering.
            const pointOnCanvas = offsetPoint(
              interactionSession.interactionData.dragStart,
              interactionSession.interactionData.drag,
            )

            const siblingsOfTarget = MetadataUtils.getChildrenPaths(
              strategyState.startingMetadata,
              newParent,
            )

            const newIndex = getReorderIndex(
              strategyState.startingMetadata,
              siblingsOfTarget,
              pointOnCanvas,
            )
            commands = [
              ...commandsBeforeReorder,
              reorderElement('permanent', newPath, newIndex),
              ...commandsAfterReorder,
            ]
          } else {
            commands = [...commandsBeforeReorder, ...commandsAfterReorder]
          }

          return {
            commands: commands,
            customState: strategyState.customStrategyState,
          }
        }
      }
      return emptyStrategyApplicationResult
    })
  },
}
