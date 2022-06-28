import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { CSSCursor } from '../canvas-types'
import { getReparentTarget } from '../canvas-utils'
import { reparentElement } from '../commands/reparent-element-command'
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
  getFileOfElement,
} from './shared-absolute-move-strategy-helpers'

export const absoluteReparentStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_REPARENT',
  name: 'Reparent Absolute Elements',
  isApplicable: (canvasState, interactionState, metadata) => {
    if (
      canvasState.selectedElements.length > 0 &&
      interactionState != null &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.modifiers.cmd
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
  fitness: (canvasState, interactionState) => {
    if (
      canvasState.selectedElements.length > 0 &&
      interactionState.activeControl.type === 'BOUNDING_AREA' &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.modifiers.cmd &&
      interactionState.interactionData.drag != null
    ) {
      return 2
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

    const { selectedElements, scale, canvasOffset, projectContents, openFile } = canvasState
    const filteredSelectedElements = getDragTargets(selectedElements)

    const reparentResult = getReparentTarget(
      filteredSelectedElements,
      filteredSelectedElements,
      strategyState.startingMetadata,
      [],
      scale,
      canvasOffset,
      projectContents,
      openFile,
      strategyState.startingAllElementProps,
    )
    const newParent = reparentResult.newParent
    const moveCommands = absoluteMoveStrategy.apply(canvasState, interactionState, strategyState)
    const providesBoundsForChildren = MetadataUtils.findElementByElementPath(
      strategyState.startingMetadata,
      newParent,
    )?.specialSizeMeasurements.providesBoundsForChildren

    if (reparentResult.shouldReparent && newParent != null && providesBoundsForChildren) {
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
          commands: [...offsetCommands, reparentElement('permanent', selectedElement, newParent)],
        }
      })

      const newPaths = commands.map((c) => c.newPath)

      return {
        commands: [
          ...moveCommands.commands,
          ...commands.flatMap((c) => c.commands),
          updateSelectedViews('permanent', newPaths),
          setElementsToRerenderCommand(newPaths),
          setCursorCommand('transient', CSSCursor.Move),
        ],
        customState: null,
      }
    } else {
      return moveCommands
    }
  },
}
