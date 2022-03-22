import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { getReparentTarget } from '../canvas-utils'
import { reparentElement } from '../commands/reparent-element-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { CanvasStrategy } from './canvas-strategy-types'
import { getAbsoluteOffsetCommandsForSelectedElement } from './shared-absolute-move-strategy-helpers'

export const absoluteReparentStrategy: CanvasStrategy = {
  id: 'ABSOLUTE_REPARENT',
  name: 'Reparent Absolute Elements',
  isApplicable: (canvasState, interactionState, metadata) => {
    if (
      canvasState.selectedElements.length === 1 &&
      interactionState != null &&
      interactionState.interactionData.modifiers.cmd
    ) {
      const selectedMetadata = MetadataUtils.findElementByElementPath(
        metadata,
        canvasState.selectedElements[0],
      )
      return selectedMetadata?.specialSizeMeasurements.position === 'absolute'
    }
    return false
  },
  controlsToRender: [],
  fitness: (canvasState, interactionState) => {
    if (
      canvasState.selectedElements.length === 1 &&
      interactionState.interactionData.modifiers.cmd &&
      interactionState.interactionData.type === 'DRAG' &&
      interactionState.interactionData.dragThresholdPassed
    ) {
      return 999
    }
    return 0
  },
  apply: (canvasState, interactionState, strategyState) => {
    const reparentResult = getReparentTarget(
      canvasState.selectedElements,
      canvasState.selectedElements,
      strategyState.startingMetadata,
      [],
      canvasState.scale,
      canvasState.canvasOffset,
      canvasState.projectContents,
      canvasState.openFile,
    )
    const newParent = reparentResult.newParent
    const moveCommands = absoluteMoveStrategy.apply(canvasState, interactionState, strategyState)

    if (reparentResult.shouldReparent && newParent != null) {
      const target = canvasState.selectedElements[0]
      const newPath = EP.appendToPath(newParent, EP.toUid(canvasState.selectedElements[0]))

      const offsetCommands = getAbsoluteOffsetCommandsForSelectedElement(
        target,
        newParent,
        strategyState,
        canvasState,
      )

      return [
        ...moveCommands,
        ...offsetCommands,
        reparentElement('permanent', target, newParent),
        updateSelectedViews('permanent', [newPath]),
      ]
    } else {
      return moveCommands
    }
  },
}
