import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import {
  isRootElementOfInstance,
  parentPath,
  pathsEqual,
} from '../../../../core/shared/element-path'
import { CanvasPoint, offsetPoint } from '../../../../core/shared/math-utils'
import { memoize } from '../../../../core/shared/memoize'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { arrayEquals, assertNever } from '../../../../core/shared/utils'
import { CanvasStrategyFactory, MetaCanvasStrategy } from '../canvas-strategies'
import {
  CustomStrategyState,
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
  isInsertionSubjects,
  isTargetPaths,
} from '../canvas-strategy-types'
import { AllowSmallerParent, InteractionSession } from '../interaction-state'
import { baseAbsoluteReparentStrategy } from './absolute-reparent-strategy'
import { baseFlexReparentToAbsoluteStrategy } from './flex-reparent-to-absolute-strategy'
import { baseReparentAsStaticStrategy } from './reparent-as-static-strategy'
import {
  findReparentStrategies,
  getReparentTargetUnified,
  ReparentStrategy,
  reparentSubjectsForInteractionTarget,
  ReparentTarget,
} from './reparent-strategy-helpers'
import { getDragTargets } from './shared-move-strategies-helpers'

interface ReparentFactoryAndDetails {
  targetParent: ElementPath
  targetIndex: number | null
  strategyType: ReparentStrategy // FIXME horrible name
  targetParentDisplayType: 'flex' | 'flow' // should this be here?
  fitness: number
  factory: CanvasStrategyFactory
}

export function getApplicableReparentFactories(
  canvasState: InteractionCanvasState,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  allDraggedElementsAbsolute: boolean,
  allowSmallerParent: AllowSmallerParent,
): Array<ReparentFactoryAndDetails> {
  const reparentStrategies = findReparentStrategies(
    canvasState,
    cmdPressed,
    pointOnCanvas,
    allowSmallerParent,
  )

  const factories: Array<ReparentFactoryAndDetails> = reparentStrategies.map((result) => {
    switch (result.strategy) {
      case 'REPARENT_AS_ABSOLUTE': {
        const fitness = result.isFallback ? 2 : 3
        if (allDraggedElementsAbsolute) {
          return {
            targetParent: result.target.newParent,
            targetIndex: null,
            strategyType: result.strategy,
            targetParentDisplayType: 'flow',
            fitness: fitness,
            factory: baseAbsoluteReparentStrategy(result.target, fitness),
          }
        } else {
          return {
            targetParent: result.target.newParent,
            targetIndex: null,
            strategyType: result.strategy,
            targetParentDisplayType: 'flow',
            fitness: fitness,
            factory: baseFlexReparentToAbsoluteStrategy(result.target, fitness),
          }
        }
      }
      case 'REPARENT_AS_STATIC': {
        const fitness = result.isFallback ? 2 : 3

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

function getStartingTargetParentsToFilterOutInner(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
): ReparentTarget | null {
  if (isInsertionSubjects(canvasState.interactionTarget)) {
    return null
  }

  const interactionData = interactionSession.interactionData
  if (interactionData.type !== 'DRAG') {
    throw new Error(
      `getStartingTargetParentsToFilterOut should only be called from a DRAG type interaction, not a ${interactionData.type} type interaction`,
    )
  }

  const pointOnCanvas = interactionData.originalDragStart
  const allowSmallerParent = interactionData.modifiers.cmd
    ? 'allow-smaller-parent'
    : 'disallow-smaller-parent'

  const reparentSubjects = reparentSubjectsForInteractionTarget(canvasState.interactionTarget)

  return getReparentTargetUnified(
    reparentSubjects,
    pointOnCanvas,
    interactionData.modifiers.cmd,
    canvasState,
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    allowSmallerParent,
  )
}

function isCanvasState(
  value: InteractionCanvasState | InteractionSession,
): value is InteractionCanvasState {
  return (value as InteractionCanvasState).startingMetadata != null
}

const getStartingTargetParentsToFilterOut = memoize(getStartingTargetParentsToFilterOutInner, {
  maxSize: 10,
  equals: (
    l: InteractionCanvasState | InteractionSession,
    r: InteractionCanvasState | InteractionSession,
  ) => {
    // We only need to re-calculate the targets to filter if the interaction targets or starting metadata have changed
    if (isCanvasState(l) && isCanvasState(r)) {
      if (isTargetPaths(l.interactionTarget) && isTargetPaths(r.interactionTarget)) {
        const lTargets = getTargetPathsFromInteractionTarget(l.interactionTarget)
        const rTargets = getTargetPathsFromInteractionTarget(r.interactionTarget)
        return (
          l.startingMetadata === r.startingMetadata && arrayEquals(lTargets, rTargets, pathsEqual)
        )
      }
    }

    return true
  },
})

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

  const startingTargetToFilter = getStartingTargetParentsToFilterOut(
    canvasState,
    interactionSession,
  )

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
    cmdPressed ? 'allow-smaller-parent' : 'disallow-smaller-parent',
  )

  const targetIsValid = (target: ElementPath): boolean => {
    if (existingParents.some((existingParent) => pathsEqual(target, existingParent))) {
      return false
    } else if (startingTargetToFilter == null) {
      return true
    } else {
      const targetToFilter = startingTargetToFilter.newParent ?? null
      return !pathsEqual(target, targetToFilter)
    }
  }

  const filteredReparentFactories = factories.filter((reparentStrategy) =>
    targetIsValid(reparentStrategy.targetParent),
  )

  return mapDropNulls(
    ({ factory }) => factory(canvasState, interactionSession, customStrategyState),
    filteredReparentFactories,
  )
}
