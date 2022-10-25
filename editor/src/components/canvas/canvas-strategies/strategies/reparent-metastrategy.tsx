import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import {
  isRootElementOfInstance,
  parentPath,
  pathsEqual,
} from '../../../../core/shared/element-path'
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
import { baseFlexReparentToAbsoluteStrategy } from './flex-reparent-to-absolute-strategy'
import { baseReparentAsStaticStrategy } from './reparent-as-static-strategy'
import { findReparentStrategies, ReparentStrategy } from './reparent-strategy-helpers'
import { getDragTargets } from './shared-move-strategies-helpers'

interface ReparentFactoryAndDetails {
  targetParent: ElementPath
  targetIndex: number | null
  strategyType: ReparentStrategy // FIXME horrible name
  missingBoundsHandling: MissingBoundsHandling
  targetParentDisplayType: 'flex' | 'flow' // should this be here?
  fitness: number
  factory: CanvasStrategyFactory
}

export function getApplicableReparentFactories(
  canvasState: InteractionCanvasState,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  allDraggedElementsAbsolute: boolean,
): Array<ReparentFactoryAndDetails> {
  const reparentStrategies = findReparentStrategies(canvasState, cmdPressed, pointOnCanvas)

  const factories: Array<ReparentFactoryAndDetails> = reparentStrategies.map((result) => {
    const missingBoundsHandling: MissingBoundsHandling = result.missingBoundsHandling
    switch (result.strategy) {
      case 'REPARENT_AS_ABSOLUTE': {
        const fitness =
          missingBoundsHandling === 'allow-missing-bounds' ? 0.5 : result.isFallback ? 2 : 3
        if (allDraggedElementsAbsolute) {
          return {
            targetParent: result.target.newParent,
            targetIndex: null,
            strategyType: result.strategy,
            missingBoundsHandling: result.missingBoundsHandling,
            targetParentDisplayType: 'flow',
            fitness: fitness,
            factory: baseAbsoluteReparentStrategy(result.target, missingBoundsHandling, fitness),
          }
        } else {
          return {
            targetParent: result.target.newParent,
            targetIndex: null,
            strategyType: result.strategy,
            missingBoundsHandling: result.missingBoundsHandling,
            targetParentDisplayType: 'flow',
            fitness: fitness,
            factory: baseFlexReparentToAbsoluteStrategy(
              result.target,
              missingBoundsHandling,
              fitness,
            ),
          }
        }
      }
      case 'REPARENT_AS_STATIC': {
        const fitness = 3

        const targetParentDisplayType =
          MetadataUtils.findElementByElementPath(
            canvasState.startingMetadata,
            result.target.newParent,
          )?.specialSizeMeasurements.display === 'flex'
            ? 'flex'
            : 'flow'

        return {
          targetParent: result.target.newParent,
          targetIndex: result.target.newIndex,
          strategyType: result.strategy,
          missingBoundsHandling: result.missingBoundsHandling,
          targetParentDisplayType: targetParentDisplayType,
          fitness: fitness,
          factory: baseReparentAsStaticStrategy(result.target, fitness, targetParentDisplayType),
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

  // TODO delete me as soon as Balint's PR is merged
  const allDraggedElementsAbsolute = reparentSubjects.every((element) =>
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
    ),
  )

  const anyDraggedElementsRootElements = reparentSubjects.some(isRootElementOfInstance)

  if (anyDraggedElementsRootElements) {
    return []
  }

  const existingParents = reparentSubjects.map(parentPath)
  const startingTargetsToFilter = interactionSession.startingTargetParentsToFilterOut

  const pointOnCanvas = offsetPoint(
    interactionSession.interactionData.originalDragStart,
    interactionSession.interactionData.drag,
  )

  const cmdPressed = interactionSession.interactionData.modifiers.cmd
  const factories = getApplicableReparentFactories(
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
