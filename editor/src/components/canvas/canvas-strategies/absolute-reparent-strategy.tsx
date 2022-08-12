import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { CSSCursor } from '../canvas-types'
import { getReparentTarget } from '../canvas-utils'
import { setCursorCommand } from '../commands/set-cursor-command'
import { setElementsToRerenderCommand } from '../commands/set-elements-to-rerender-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { CanvasStrategy, emptyStrategyApplicationResult } from './canvas-strategy-types'
import {
  getAbsoluteOffsetCommandsForSelectedElement,
  getDragTargets,
} from './shared-absolute-move-strategy-helpers'
import { ifAllowedToReparent, isAllowedToReparent } from './reparent-helpers'
import { findReparentStrategy, newGetReparentTarget } from './reparent-strategy-helpers'
import { offsetPoint } from '../../../core/shared/math-utils'
import { getReparentCommands } from './reparent-utils'

export const absoluteReparentStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_REPARENT',
  name: 'Reparent Absolute Elements',
  isApplicable: (canvasState, interactionState, metadata) => {
    if (
      canvasState.selectedElements.length > 0 &&
      interactionState != null &&
      interactionState.interactionData.type === 'DRAG'
    ) {
      const filteredSelectedElements = getDragTargets(canvasState.selectedElements)
      return filteredSelectedElements.every((element) => {
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, element)

        return elementMetadata?.specialSizeMeasurements.position === 'absolute'
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
  fitness: (canvasState, interactionState, strategyState) => {
    // All 4 reparent strategies use the same fitness function findReparentStrategy
    const reparentStrategy = findReparentStrategy(
      canvasState,
      interactionState,
      strategyState,
      true,
    ).strategy
    if (reparentStrategy === 'ABSOLUTE_REPARENT_TO_ABSOLUTE') {
      return 3
    }
    return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    if (
      interactionState.interactionData.type != 'DRAG' ||
      interactionState.interactionData.drag == null
    ) {
      return emptyStrategyApplicationResult
    }

    const { selectedElements, projectContents, openFile, nodeModules } = canvasState
    const filteredSelectedElements = getDragTargets(selectedElements)

    const pointOnCanvas = offsetPoint(
      interactionState.interactionData.originalDragStart,
      interactionState.interactionData.drag,
    )

    const reparentResult = newGetReparentTarget(
      filteredSelectedElements,
      pointOnCanvas,
      interactionState.interactionData.modifiers.cmd,
      canvasState,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )
    const newParent = reparentResult.newParent
    const moveCommands = absoluteMoveStrategy.apply(canvasState, interactionState, strategyState)
    const providesBoundsForAbsoluteChildren =
      MetadataUtils.findElementByElementPath(strategyState.startingMetadata, newParent)
        ?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false
    const parentIsStoryboard = newParent == null ? false : EP.isStoryboardPath(newParent)
    const allowedToReparent = filteredSelectedElements.every((selectedElement) => {
      return isAllowedToReparent(canvasState, strategyState, selectedElement)
    })

    if (
      reparentResult.shouldReparent &&
      newParent != null &&
      (providesBoundsForAbsoluteChildren || parentIsStoryboard) &&
      allowedToReparent
    ) {
      const commands = filteredSelectedElements.map((selectedElement) => {
        const offsetCommands = getAbsoluteOffsetCommandsForSelectedElement(
          selectedElement,
          newParent,
          strategyState,
          canvasState,
        )

        const newPath = EP.appendToPath(newParent, EP.toUid(selectedElement))
        return {
          newPath: newPath,
          commands: [
            ...offsetCommands,
            ...getReparentCommands(
              canvasState.builtInDependencies,
              projectContents,
              nodeModules,
              openFile,
              selectedElement,
              newParent,
            ),
          ],
        }
      })

      const newPaths = commands.map((c) => c.newPath)

      return {
        commands: [
          ...moveCommands.commands,
          ...commands.flatMap((c) => c.commands),
          updateSelectedViews('always', newPaths),
          setElementsToRerenderCommand([...newPaths, ...filteredSelectedElements]),
          setCursorCommand('mid-interaction', CSSCursor.Move),
        ],
        customState: null,
      }
    } else {
      return moveCommands
    }
  },
}
