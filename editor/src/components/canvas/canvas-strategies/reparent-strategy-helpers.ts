import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { offsetPoint } from '../../../core/shared/math-utils'
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

export function findReparentStrategy(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  strategyState: StrategyState,
): { strategy: ReparentStrategy; newParent: ElementPath } | { strategy: 'do-not-reparent' } {
  if (
    canvasState.selectedElements.length === 0 ||
    interactionState.activeControl.type !== 'BOUNDING_AREA' ||
    interactionState.interactionData.type !== 'DRAG' ||
    !interactionState.interactionData.modifiers.cmd ||
    interactionState.interactionData.drag == null // TODO delete this drag nullcheck? do we start the reparent on mouse down or mouse move beyond threshold?
  ) {
    return { strategy: 'do-not-reparent' }
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

  const reparentResult = getReparentTargetForFlexElement(
    filteredSelectedElements,
    interactionState,
    canvasState,
    strategyState,
  )

  const newParentPath = reparentResult.newParent
  const newParentMetadata = MetadataUtils.findElementByElementPath(startingMetadata, newParentPath)
  const parentProvidesBoundsForAbsoluteChildren =
    newParentMetadata?.specialSizeMeasurements.providesBoundsForAbsoluteChildren ?? false

  const parentIsFlexLayout = MetadataUtils.isFlexLayoutedContainer(newParentMetadata)
  const parentIsStoryboard = newParentPath == null ? false : EP.isStoryboardPath(newParentPath)

  if (reparentResult.shouldReparent && newParentPath != null) {
    if (allDraggedElementsAbsolute) {
      if (parentIsFlexLayout) {
        return { strategy: 'ABSOLUTE_REPARENT_TO_FLEX', newParent: newParentPath }
      }
      if (parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard) {
        return { strategy: 'ABSOLUTE_REPARENT_TO_ABSOLUTE', newParent: newParentPath }
      }
    }
    if (allDraggedElementsFlex) {
      if (parentIsFlexLayout) {
        return { strategy: 'FLEX_REPARENT_TO_FLEX', newParent: newParentPath }
      }
      if (parentProvidesBoundsForAbsoluteChildren || parentIsStoryboard) {
        return { strategy: 'FLEX_REPARENT_TO_ABSOLUTE', newParent: newParentPath }
      }
    }
  }
  return { strategy: 'do-not-reparent' }
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
  if (
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null
  ) {
    return {
      shouldReparent: false,
      newParent: null,
      shouldReorder: false,
    }
  }

  const pointOnCanvas = offsetPoint(
    interactionSession.interactionData.originalDragStart,
    interactionSession.interactionData.drag,
  )

  const reparentResult = getReparentTarget(
    filteredSelectedElements,
    filteredSelectedElements,
    strategyState.startingMetadata,
    [],
    pointOnCanvas,
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
      // Otherwise we want to use the target directly.
      // But in this case no re-ordering should be triggered, the element should just be
      // added to the end.
      return {
        shouldReparent: true,
        newParent: reparentResult.newParent,
        shouldReorder: false,
      }
    }
  }
}
