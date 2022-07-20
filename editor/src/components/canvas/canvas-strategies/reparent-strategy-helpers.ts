import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementPath } from '../../../core/shared/project-file-types'
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

export function getReparentTargetForFlexElement(
  filteredSelectedElements: Array<ElementPath>,
  interactionSession: InteractionSession,
  canvasState: InteractionCanvasState,
  strategyState: StrategyState,
): {
  shouldReparent: boolean
  newParent: ElementPath | null
  shouldReorder: boolean
} {
  const reparentResult = getReparentTarget(
    filteredSelectedElements,
    filteredSelectedElements,
    strategyState.startingMetadata,
    [],
    canvasState.scale,
    canvasState.canvasOffset,
    canvasState.projectContents,
    canvasState.openFile,
    strategyState.startingAllElementProps,
  )
  if (reparentResult.newParent == null) {
    return {
      ...reparentResult,
      shouldReorder: false,
    }
  } else {
    // The target is in a flex container, so we want the parent of the target to reparent
    // into and reordering should be triggered because the pointer is over an existing flex element.
    if (
      MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
        reparentResult.newParent,
        strategyState.startingMetadata,
      )
    ) {
      return {
        shouldReparent: true,
        newParent: EP.parentPath(reparentResult.newParent),
        shouldReorder: true,
      }
    } else {
      const metadata = MetadataUtils.findElementByElementPath(
        strategyState.startingMetadata,
        reparentResult.newParent,
      )
      // The target is a flex container, so we want to use the target directly.
      // But in this case no re-ordering should be triggered, the element should just be
      // added to the end.
      if (MetadataUtils.isFlexLayoutedContainer(metadata)) {
        return {
          shouldReparent: true,
          newParent: reparentResult.newParent,
          shouldReorder: false,
        }
      } else {
        return {
          shouldReparent: false,
          newParent: null,
          shouldReorder: false,
        }
      }
    }
  }
}
