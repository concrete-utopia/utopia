import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { CanvasStrategy } from '../../../interactions_proposal'
import { getReparentTarget } from '../canvas-utils'
import { adjustNumberProperty, reparentElement, updateSelectedViews } from '../commands/commands'
import * as EP from '../../../core/shared/element-path'
import { pointDifference, zeroCanvasRect } from '../../../core/shared/math-utils'
import { stylePropPathMappingFn } from '../../../components/inspector/common/property-path-hooks'

export const absoluteReparentStrategy: CanvasStrategy = {
  name: 'Reparent Absolute Elements',
  isApplicable: (canvasState, interactionState) => {
    if (
      canvasState.selectedElements.length === 1 &&
      interactionState != null &&
      interactionState.interactionData.modifiers.cmd
    ) {
      const metadata = MetadataUtils.findElementByElementPath(
        canvasState.metadata,
        canvasState.selectedElements[0],
      )
      return metadata?.specialSizeMeasurements.position === 'absolute'
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
      const reparentResult = getReparentTarget(
        canvasState.selectedElements,
        canvasState.selectedElements,
        canvasState.metadata,
        [],
        canvasState.scale,
        canvasState.canvasOffset,
        canvasState.projectContents,
        canvasState.openFile,
      )
      return reparentResult.shouldReparent && reparentResult.newParent != null ? 999 : 0
    }
    return 0
  },
  apply: (canvasState, interactionState, sessionState) => {
    const reparentResult = getReparentTarget(
      canvasState.selectedElements,
      canvasState.selectedElements,
      canvasState.metadata,
      [],
      canvasState.scale,
      canvasState.canvasOffset,
      canvasState.projectContents,
      canvasState.openFile,
    )
    const newParent = reparentResult.newParent
    if (newParent != null) {
      const target = canvasState.selectedElements[0]
      const newPath = EP.appendToPath(newParent, EP.toUid(canvasState.selectedElements[0]))

      const oldParentFrame =
        MetadataUtils.getFrameInCanvasCoords(EP.parentPath(target), canvasState.metadata) ??
        zeroCanvasRect
      const newParentFrame =
        MetadataUtils.getFrameInCanvasCoords(newParent, canvasState.metadata) ?? zeroCanvasRect
      const offset = pointDifference(newParentFrame, oldParentFrame)

      return [
        adjustNumberProperty(
          'permanent',
          target,
          stylePropPathMappingFn('left', ['style']),
          offset.x,
        ),
        adjustNumberProperty(
          'permanent',
          target,
          stylePropPathMappingFn('top', ['style']),
          offset.y,
        ),
        reparentElement('permanent', target, newParent),
        updateSelectedViews('permanent', [newPath]),
      ]
    }
    return []
  },
}
