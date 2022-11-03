import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'

export const SetFlexGapControlStrategyId = 'SET_FLEX_GAP_STRATEGY'

export const setFlexGapControlStrategy: CanvasStrategyFactory = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customtrategyState: CustomStrategyState,
) => {
  if (
    interactionSession != null &&
    !(
      interactionSession.interactionData.type === 'DRAG' &&
      interactionSession.activeControl.type === 'FLEX_GAP_HANDLE'
    )
  ) {
    // We don't want to include this in the strategy picker if any other interaction is active
    return null
  }

  const selectedElements = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  if (selectedElements.length !== 1) {
    return null
  }

  if (!supportsFlexGapControls(canvasState.startingMetadata, selectedElements[0])) {
    return null
  }

  return {
    id: SetFlexGapControlStrategyId,
    name: 'Set flex gap',
    controlsToRender: [],
    fitness: 1,
    apply: () => emptyStrategyApplicationResult,
  }
}

function supportsFlexGapControls(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (elementMetadata == null || elementMetadata.specialSizeMeasurements.display !== 'flex') {
    return false
  }

  const children = MetadataUtils.getChildren(metadata, elementPath)

  return children.some((c) => c.specialSizeMeasurements.parentFlexGap > 0)
}
