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

  const flexGap = maybeFlexGapFromElement(canvasState.startingMetadata, selectedElements[0])
  if (flexGap == null) {
    return null
  }

  return {
    id: SetFlexGapStrategyId,
    name: 'Set flex gap',
    controlsToRender: [
      controlWithProps({
        control: FlexGapControl,
        props: { selectedElement: selectedElements[0], gap: flexGap },
        key: 'flex-gap-resize-control',
        show: 'visible-except-when-other-strategy-is-active',
      }),
    ],
    fitness: 1,
    apply: () => emptyStrategyApplicationResult,
  }
}

function maybeFlexGapFromElement(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): number | null {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (elementMetadata == null || elementMetadata.specialSizeMeasurements.display !== 'flex') {
    return null
  }

  const children = MetadataUtils.getChildren(metadata, elementPath)
  if (children.length < 2) {
    return null
  }

  const flexGap = children[0].specialSizeMeasurements.parentFlexGap
  if (flexGap === 0) {
    return null
  }

  return flexGap
}
