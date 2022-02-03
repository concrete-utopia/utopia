import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { CanvasStrategy } from '../../../interactions_proposal'
import { getReparentTarget } from '../canvas-utils'
import { reparentElement } from '../commands/commands'

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
      interactionState.interactionData.type === 'DRAG'
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
    const newParent = reparentResult.newParent!

    return [reparentElement('permanent', canvasState.selectedElements[0], newParent)]
  },
}
