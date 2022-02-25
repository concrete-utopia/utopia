import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { CanvasStrategy } from './canvas-strategy-types'
import { getReparentTarget } from '../canvas-utils'
import { adjustNumberProperty, reparentElement, updateSelectedViews } from '../commands/commands'
import * as EP from '../../../core/shared/element-path'
import { pointDifference, zeroCanvasRect } from '../../../core/shared/math-utils'
import { stylePropPathMappingFn } from '../../../components/inspector/common/property-path-hooks'
import { absoluteMoveStrategy } from './absolute-move-strategy'

export const absoluteReparentStrategy: CanvasStrategy = {
  name: 'Reparent Absolute Elements',
  strategyGroups: new Set(),
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
  apply: (canvasState, interactionState, sessionState) => {
    const reparentResult = getReparentTarget(
      canvasState.selectedElements,
      canvasState.selectedElements,
      sessionState.startingMetadata,
      [],
      canvasState.scale,
      canvasState.canvasOffset,
      canvasState.projectContents,
      canvasState.openFile,
    )
    const newParent = reparentResult.newParent
    const moveCommands = absoluteMoveStrategy.apply(canvasState, interactionState, sessionState)

    if (newParent != null) {
      const target = canvasState.selectedElements[0]
      const newPath = EP.appendToPath(newParent, EP.toUid(canvasState.selectedElements[0]))

      const oldParentFrame =
        MetadataUtils.getFrameInCanvasCoords(
          EP.parentPath(target),
          sessionState.startingMetadata,
        ) ?? zeroCanvasRect
      const newParentFrame =
        MetadataUtils.getFrameInCanvasCoords(newParent, sessionState.startingMetadata) ??
        zeroCanvasRect
      const offset = pointDifference(newParentFrame, oldParentFrame)

      return [
        ...moveCommands,
        adjustNumberProperty(
          'permanent',
          target,
          stylePropPathMappingFn('left', ['style']),
          offset.x,
          true,
        ),
        adjustNumberProperty(
          'permanent',
          target,
          stylePropPathMappingFn('top', ['style']),
          offset.y,
          true,
        ),
        reparentElement('permanent', target, newParent),
        updateSelectedViews('permanent', [newPath]),
      ]
    } else {
      return moveCommands
    }
  },
}
