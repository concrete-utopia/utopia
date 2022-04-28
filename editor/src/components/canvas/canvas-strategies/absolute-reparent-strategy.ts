import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { getReparentTarget } from '../canvas-utils'
import { reparentElement } from '../commands/reparent-element-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { CanvasStrategy } from './canvas-strategy-types'
import {
  getAbsoluteOffsetCommandsForSelectedElement,
  getFileOfElement,
} from './shared-absolute-move-strategy-helpers'

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
    const { selectedElements, scale, canvasOffset, projectContents, openFile } = canvasState

    const reparentResult = getReparentTarget(
      selectedElements,
      selectedElements,
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

    const target = selectedElements[0]

    const targetElementFile = getFileOfElement(target, projectContents, openFile)

    const newParentFile = getFileOfElement(newParent, projectContents, openFile)

    // Currently we only support reparenting into the same file
    const reparentingToSameFile = targetElementFile === newParentFile

    if (
      reparentResult.shouldReparent &&
      newParent != null &&
      providesBoundsForChildren &&
      reparentingToSameFile
    ) {
      const newPath = EP.appendToPath(newParent, EP.toUid(target))

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
