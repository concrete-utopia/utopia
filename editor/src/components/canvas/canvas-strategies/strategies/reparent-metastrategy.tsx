import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { parentPath, pathsEqual } from '../../../../core/shared/element-path'
import { CanvasPoint, offsetPoint } from '../../../../core/shared/math-utils'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { CanvasStrategyFactory, MetaCanvasStrategy } from '../canvas-strategies'
import {
  CustomStrategyState,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { InteractionSession, MissingBoundsHandling } from '../interaction-state'
import { baseAbsoluteReparentStrategy } from './absolute-reparent-strategy'
import { baseAbsoluteReparentToFlexStrategy } from './absolute-reparent-to-flex-strategy'
import { baseFlexReparentToAbsoluteStrategy } from './flex-reparent-to-absolute-strategy'
import { baseFlexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
import { findReparentStrategies, ReparentStrategy } from './reparent-strategy-helpers'
import { getDragTargets } from './shared-move-strategies-helpers'

export function getReparentFactories(
  canvasState: InteractionCanvasState,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  allDraggedElementsAbsolute: boolean,
): Array<{
  targetParent: ElementPath
  targetIndex: number | null
  strategyType: ReparentStrategy // FIXME horrible name
  missingBoundsHandling: MissingBoundsHandling
  factory: CanvasStrategyFactory
}> {
  const reparentStrategies = findReparentStrategies(canvasState, cmdPressed, pointOnCanvas)

  const factories: Array<{
    // TODO share type
    targetParent: ElementPath
    targetIndex: number | null
    strategyType: ReparentStrategy // FIXME horrible name
    missingBoundsHandling: MissingBoundsHandling
    factory: CanvasStrategyFactory
  }> = reparentStrategies.map((result) => {
    const missingBoundsHandling: MissingBoundsHandling = result.missingBoundsHandling
    switch (result.strategy) {
      case 'REPARENT_TO_ABSOLUTE':
        if (allDraggedElementsAbsolute) {
          return {
            targetParent: result.target.newParent,
            targetIndex: null,
            strategyType: result.strategy,
            missingBoundsHandling: result.missingBoundsHandling,
            factory: baseAbsoluteReparentStrategy(
              result.target,
              missingBoundsHandling,
              result.isFallback,
            ),
          }
        } else {
          return {
            targetParent: result.target.newParent,
            targetIndex: null,
            strategyType: result.strategy,
            missingBoundsHandling: result.missingBoundsHandling,
            factory: baseFlexReparentToAbsoluteStrategy(
              result.target,
              missingBoundsHandling,
              result.isFallback,
            ),
          }
        }
      case 'REPARENT_TO_FLEX':
        if (allDraggedElementsAbsolute) {
          return {
            targetParent: result.target.newParent,
            targetIndex: result.target.newIndex,
            strategyType: result.strategy,
            missingBoundsHandling: result.missingBoundsHandling,
            factory: baseAbsoluteReparentToFlexStrategy(result.target),
          }
        } else {
          return {
            targetParent: result.target.newParent,
            targetIndex: result.target.newIndex,
            strategyType: result.strategy,
            missingBoundsHandling: result.missingBoundsHandling,
            factory: baseFlexReparentToFlexStrategy(result.target),
          }
        }
      default:
        assertNever(result.strategy)
    }
  })

  return factories
}

export const reparentMetaStrategy: MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  customStrategyState: CustomStrategyState,
) => {
  const reparentSubjects = getDragTargets(
    getTargetPathsFromInteractionTarget(canvasState.interactionTarget),
  )

  if (
    reparentSubjects.length === 0 ||
    interactionSession == null ||
    interactionSession.activeControl.type !== 'BOUNDING_AREA' ||
    interactionSession.interactionData.type !== 'DRAG' ||
    interactionSession.interactionData.drag == null ||
    interactionSession.interactionData.modifiers.alt
  ) {
    return []
  }

  const allDraggedElementsAbsolute = reparentSubjects.every((element) =>
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
    ),
  )

  const allDraggedElementsFlex = reparentSubjects.every((element) =>
    MetadataUtils.isParentYogaLayoutedContainerAndElementParticipatesInLayout(
      element,
      canvasState.startingMetadata,
    ),
  )

  if (!(allDraggedElementsAbsolute || allDraggedElementsFlex)) {
    return []
  }

  const existingParents = reparentSubjects.map(parentPath)
  const startingTargetsToFilter = interactionSession.startingTargetParentsToFilterOut

  const pointOnCanvas = offsetPoint(
    interactionSession.interactionData.originalDragStart,
    interactionSession.interactionData.drag,
  )

  const cmdPressed = interactionSession.interactionData.modifiers.cmd
  const factories = getReparentFactories(
    canvasState,
    pointOnCanvas,
    cmdPressed,
    allDraggedElementsAbsolute,
  )

  const targetIsValid = (
    target: ElementPath,
    missingBoundsHandling: MissingBoundsHandling,
  ): boolean => {
    if (existingParents.some((existingParent) => pathsEqual(target, existingParent))) {
      return false
    } else if (startingTargetsToFilter == null) {
      return true
    } else {
      const targetToFilter = startingTargetsToFilter[missingBoundsHandling]?.newParent ?? null
      return !pathsEqual(target, targetToFilter)
    }
  }

  const filteredReparentFactories = factories.filter((reparentStrategy) =>
    targetIsValid(reparentStrategy.targetParent, reparentStrategy.missingBoundsHandling),
  )

  return mapDropNulls(
    ({ factory }) => factory(canvasState, interactionSession, customStrategyState),
    filteredReparentFactories,
  )
}
