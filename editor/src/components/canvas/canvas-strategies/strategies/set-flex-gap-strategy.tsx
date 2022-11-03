import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { FlexGapControl } from '../../controls/select-mode/flex-gap-control'
import { CanvasStrategyFactory } from '../canvas-strategies'
import {
  controlWithProps,
  CustomStrategyState,
  emptyStrategyApplicationResult,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { InteractionSession } from '../interaction-state'

export const SetFlexGapStrategyId = 'SET_FLEX_GAP_STRATEGY'

export const setFlexGapStrategy: CanvasStrategyFactory = (
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
    id: SetFlexGapStrategyId,
    name: 'Set flex gap',
    controlsToRender: [
      controlWithProps({
        control: FlexGapControl,
        props: { selectedElement: selectedElements[0] },
        key: 'flex-gap-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
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

  return children.length > 1 && children.some((c) => c.specialSizeMeasurements.parentFlexGap > 0)
}
