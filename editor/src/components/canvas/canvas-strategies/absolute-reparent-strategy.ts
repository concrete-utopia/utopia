import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { getReparentTarget } from '../canvas-utils'
import { reparentElement } from '../commands/reparent-element-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
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
  controlsToRender: [],
  fitness: (canvasState, interactionState) => {
    if (
      canvasState.selectedElements.length > 0 &&
      interactionState.interactionData.modifiers.cmd &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.dragThresholdPassed
    ) {
      return 2
    }
    return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
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

      return {
        commands: [
          ...moveCommands.commands,
          ...commands.flatMap((c) => c.commands),
          updateSelectedViews(
            'permanent',
            commands.map((c) => c.newPath),
          ),
        ],
        customState: null,
      }
    } else {
      return moveCommands
    }
  },
}
