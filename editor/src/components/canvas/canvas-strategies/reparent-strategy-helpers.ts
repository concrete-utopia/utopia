import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { getReparentTarget } from '../canvas-utils'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { InteractionCanvasState, emptyStrategyApplicationResult } from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { getDragTargets } from './shared-absolute-move-strategy-helpers'

type ReparentStrategy =
  | 'FLEX_REPARENT_TO_ABSOLUTE'
  | 'FLEX_REPARENT_TO_FLEX'
  | 'ABSOLUTE_REPARENT_TO_ABSOLUTE'
  | 'ABSOLUTE_REPARENT_TO_FLEX'
  | 'do-not-reparent'

export function findReparentStrategy(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyState,
): ReparentStrategy {
  if (
    interactionState.interactionData.type !== 'DRAG' ||
    interactionState.activeControl.type !== 'BOUNDING_AREA' ||
    interactionState.interactionData.drag == null
  ) {
    return 'do-not-reparent'
  }

  const { selectedElements, scale, canvasOffset, projectContents, openFile } = canvasState
  const startingMetadata = strategyState.startingMetadata
  const filteredSelectedElements = getDragTargets(selectedElements)

  const allDraggedElementsFlex = filteredSelectedElements.every((element) =>
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      element,
      startingMetadata,
    ),
  )
  const allDraggedElementsAbsolute = filteredSelectedElements.every((element) =>
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(startingMetadata, element),
    ),
  )

  const reparentResult = getReparentTarget(
    filteredSelectedElements,
    filteredSelectedElements,
    startingMetadata,
    [],
    scale,
    canvasOffset,
    projectContents,
    openFile,
    strategyState.startingAllElementProps,
  )
  const newParentPath = reparentResult.newParent
  const newParentMetadata = MetadataUtils.findElementByElementPath(startingMetadata, newParentPath)
  const parentProvidesBoundsForAbsoluteChildren =
    newParentMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)
  const parentIsStoryboard = newParentPath == null ? false : EP.isStoryboardPath(newParentPath)

  if (reparentResult.shouldReparent && newParentPath != null) {
    if (allDraggedElementsAbsolute) {
      if (parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard) {
        return 'ABSOLUTE_REPARENT_TO_ABSOLUTE'
      }
      if (parentIsFlexLayout) {
        return 'ABSOLUTE_REPARENT_TO_FLEX'
      }
    }
    if (allDraggedElementsFlex) {
      if (parentIsFlexLayout) {
        return 'FLEX_REPARENT_TO_FLEX'
      }
      if (parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard) {
        return 'FLEX_REPARENT_TO_ABSOLUTE'
      }
    }
  }
  return 'do-not-reparent'
}
