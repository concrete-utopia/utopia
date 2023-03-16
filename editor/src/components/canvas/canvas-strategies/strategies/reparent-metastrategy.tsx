import {
  ElementSupportsChildren,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { allElemsEqual, mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import { CanvasPoint, offsetPoint } from '../../../../core/shared/math-utils'
import { memoize } from '../../../../core/shared/memoize'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { arrayEquals, assertNever } from '../../../../core/shared/utils'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
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
import { appendCommandsToApplyResult } from './ancestor-metastrategy'
import { baseFlexReparentToAbsoluteStrategy } from './flex-reparent-to-absolute-strategy'
import { replaceContentAffectingPathsWithTheirChildrenRecursive } from './group-like-helpers'
import { baseReparentAsStaticStrategy } from './reparent-as-static-strategy'
import {
  findReparentStrategies,
  ReparentStrategy,
  reparentSubjectsForInteractionTarget,
  ReparentTarget,
} from './reparent-helpers/reparent-strategy-helpers'
import { getReparentTargetUnified } from './reparent-helpers/reparent-strategy-parent-lookup'
import { flattenSelection } from './shared-move-strategies-helpers'

interface ReparentFactoryAndDetails {
  targetParent: ElementPath
  targetIndex: number | null
  strategyType: ReparentStrategy // FIXME horrible name
  targetParentDisplayType: 'flex' | 'flow' // should this be here?
  fitness: number
  dragType: 'absolute' | 'static'
  factory: CanvasStrategyFactory
}

export function getApplicableReparentFactories(
  canvasState: InteractionCanvasState,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  allDraggedElementsAbsolute: boolean,
  allowSmallerParent: AllowSmallerParent,
  elementSupportsChildren: Array<ElementSupportsChildren> = ['supportsChildren'],
): Array<ReparentFactoryAndDetails> {
  const reparentStrategies = findReparentStrategies(
    canvasState,
    cmdPressed,
    pointOnCanvas,
    allowSmallerParent,
    elementSupportsChildren,
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
            dragType: 'absolute',
            factory: baseAbsoluteReparentStrategy(result.target, fitness),
          }
        } else {
          return {
            targetParent: result.target.newParent,
            targetIndex: null,
            strategyType: result.strategy,
            targetParentDisplayType: 'flow',
            fitness: fitness,
            dragType: 'absolute',
            factory: baseFlexReparentToAbsoluteStrategy(result.target, fitness),
          }
        }
      }
      case 'REPARENT_AS_STATIC': {
        const fitness = result.isFallback ? 2 : 3

        const parentLayouSystems = MetadataUtils.findLayoutSystemForChildren(
          canvasState.startingMetadata,
          result.target.newParent,
        )

        const targetParentDisplayType = parentLayouSystems.at(0) === 'flex' ? 'flex' : 'flow'

        return {
          targetParent: result.target.newParent,
          targetIndex: result.target.newIndex,
          strategyType: result.strategy,
          targetParentDisplayType: targetParentDisplayType,
          fitness: fitness,
          dragType: 'static',
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
  elementSupportsChildren: Array<ElementSupportsChildren> = ['supportsChildren'],
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
    elementSupportsChildren,
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
          l.startingMetadata === r.startingMetadata &&
          arrayEquals(lTargets, rTargets, EP.pathsEqual)
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
  const reparentSubjects = flattenSelection(
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

  const allDraggedElementsAbsolute = replaceContentAffectingPathsWithTheirChildrenRecursive(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    reparentSubjects,
  ).every((element) =>
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(canvasState.startingMetadata, element),
    ),
  )

  const anyDraggedElementsRootElements = reparentSubjects.some(EP.isRootElementOfInstance)

  if (anyDraggedElementsRootElements) {
    return []
  }

  const existingParents = reparentSubjects.map(EP.parentPath)

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
    if (existingParents.some((existingParent) => EP.pathsEqual(target, existingParent))) {
      return false
    } else if (startingTargetToFilter == null) {
      return true
    } else {
      const targetToFilter = startingTargetToFilter.newParent ?? null
      return !EP.pathsEqual(target, targetToFilter)
    }
  }

  const filteredReparentFactories = factories.filter((reparentStrategy) =>
    targetIsValid(reparentStrategy.targetParent),
  )

  return mapDropNulls(({ factory, dragType, targetParent }) => {
    const strategy = factory(canvasState, interactionSession, customStrategyState)
    if (strategy == null) {
      return null
    }
    const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    const isReparentedWithinComponent = targets.some((target) =>
      EP.pathsEqual(EP.getContainingComponent(target), EP.getContainingComponent(targetParent)),
    )
    const indicatorCommand = wildcardPatch('mid-interaction', {
      canvas: {
        controls: {
          dragToMoveIndicatorFlags: {
            $set: {
              showIndicator: true,
              dragType: dragType,
              reparent: isReparentedWithinComponent ? 'same-component' : 'different-component',
              ancestor: false,
            },
          },
        },
      },
    })
    return {
      ...strategy,
      apply: appendCommandsToApplyResult(strategy.apply, [], [indicatorCommand]),
    }
  }, filteredReparentFactories)
}
