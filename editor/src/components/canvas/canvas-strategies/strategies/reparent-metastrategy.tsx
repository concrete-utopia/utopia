import type { ElementSupportsChildren } from '../../../../core/model/element-template-utils'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import * as EP from '../../../../core/shared/element-path'
import type { CanvasPoint } from '../../../../core/shared/math-utils'
import { offsetPoint } from '../../../../core/shared/math-utils'
import { memoize } from '../../../../core/shared/memoize'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { arrayEqualsByValue, assertNever } from '../../../../core/shared/utils'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import type { CanvasStrategyFactory, MetaCanvasStrategy } from '../canvas-strategies'
import type { CustomStrategyState, InteractionCanvasState } from '../canvas-strategy-types'
import {
  getTargetPathsFromInteractionTarget,
  isInsertionSubjects,
  isTargetPaths,
} from '../canvas-strategy-types'
import type { AllowSmallerParent, InteractionSession } from '../interaction-state'
import { baseAbsoluteReparentStrategy } from './absolute-reparent-strategy'
import { appendCommandsToApplyResult } from './ancestor-metastrategy'
import { baseFlexReparentToAbsoluteStrategy } from './flex-reparent-to-absolute-strategy'
import { replaceFragmentLikePathsWithTheirChildrenRecursive } from './fragment-like-helpers'
import { baseReparentAsStaticStrategy } from './reparent-as-static-strategy'
import type { ReparentStrategy, ReparentTarget } from './reparent-helpers/reparent-strategy-helpers'
import { getExistingElementsFromReparentSubjects } from './reparent-helpers/reparent-strategy-helpers'
import {
  findReparentStrategies,
  reparentSubjectsForInteractionTarget,
} from './reparent-helpers/reparent-strategy-helpers'
import { getReparentTargetUnified } from './reparent-helpers/reparent-strategy-parent-lookup'
import { flattenSelection } from './shared-move-strategies-helpers'
import type { InsertionPath } from '../../../editor/store/insertion-path'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import { treatElementAsGroupLike } from './group-helpers'
import { PropertyControlsInfo } from '../../../custom-code/code-file'

interface ReparentFactoryAndDetails {
  targetParent: InsertionPath
  targetIndex: number | null
  strategyType: ReparentStrategy // FIXME horrible name
  targetParentDisplayType: 'flex' | 'flow' // should this be here?
  fitness: number
  dragType: 'absolute' | 'static'
  factory: CanvasStrategyFactory
}

const DefaultReparentWeight = 4
const FallbackReparentWeight = DefaultReparentWeight - 1
const FlowReparentWeight = FallbackReparentWeight - 1

export function getApplicableReparentFactories(
  canvasState: InteractionCanvasState,
  pointOnCanvas: CanvasPoint,
  cmdPressed: boolean,
  allDraggedElementsAbsolute: boolean,
  allowSmallerParent: AllowSmallerParent,
  customStrategyState: CustomStrategyState,
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
        const fitness = result.isFallback ? FallbackReparentWeight : DefaultReparentWeight
        if (allDraggedElementsAbsolute) {
          return {
            targetParent: result.target.newParent,
            targetIndex: null,
            strategyType: result.strategy,
            targetParentDisplayType: 'flow',
            fitness: fitness,
            dragType: 'absolute',
            factory: baseAbsoluteReparentStrategy(result.target, fitness, customStrategyState),
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
        const parentLayoutSystem = MetadataUtils.findLayoutSystemForChildren(
          canvasState.startingMetadata,
          canvasState.startingElementPathTree,
          result.target.newParent.intendedParentPath,
        )
        const targetParentDisplayType = parentLayoutSystem === 'flex' ? 'flex' : 'flow'

        // We likely never want flow insertion or re-parenting to be the default
        const fitness =
          targetParentDisplayType === 'flow'
            ? FlowReparentWeight
            : result.isFallback
            ? FallbackReparentWeight
            : DefaultReparentWeight

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
): Array<ReparentTarget> {
  if (isInsertionSubjects(canvasState.interactionTarget)) {
    return []
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

  let result: Array<ReparentTarget> = []
  const regularReparentTarget = getReparentTargetUnified(
    reparentSubjects,
    pointOnCanvas,
    interactionData.modifiers.cmd,
    canvasState,
    canvasState.startingMetadata,
    canvasState.startingElementPathTree,
    canvasState.startingAllElementProps,
    allowSmallerParent,
    elementSupportsChildren,
    canvasState.propertyControlsInfo,
  )
  if (regularReparentTarget != null) {
    result.push(regularReparentTarget)
  }

  // If the target item(s) are in a group, prevent the group parent from being a target for reparenting.
  const targetElements = getExistingElementsFromReparentSubjects(reparentSubjects)
  for (const targetElement of targetElements) {
    const parentPath = EP.parentPath(targetElement)
    if (treatElementAsGroupLike(canvasState.startingMetadata, parentPath)) {
      const groupParentPath = EP.parentPath(parentPath)
      result.push({
        shouldReparent: false,
        newParent: childInsertionPath(groupParentPath),
        shouldShowPositionIndicator: false,
        newIndex: 0,
        shouldConvertToInline: 'do-not-convert',
        defaultReparentType: 'REPARENT_AS_STATIC',
      })
    }
  }

  return result
}

function isCanvasState(
  value: InteractionCanvasState | InteractionSession,
): value is InteractionCanvasState {
  return (value as InteractionCanvasState).startingMetadata != null
}

const getStartingTargetParentsToFilterOut = memoize(getStartingTargetParentsToFilterOutInner, {
  maxSize: 10,
  matchesArg: (
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
          arrayEqualsByValue(lTargets, rTargets, EP.pathsEqual)
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

  const allDraggedElementsAbsolute = replaceFragmentLikePathsWithTheirChildrenRecursive(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
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

  const existingParents = reparentSubjects.map((p) => EP.dynamicPathToStaticPath(EP.parentPath(p)))

  const startingTargetsToFilter = getStartingTargetParentsToFilterOut(
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
    customStrategyState,
  )

  const targetIsValid = (target: ElementPath): boolean => {
    if (existingParents.some((existingParent) => EP.pathsEqual(target, existingParent))) {
      return false
    } else {
      return startingTargetsToFilter.every((startingTargetToFilter) => {
        const targetToFilter = startingTargetToFilter.newParent
        return !EP.pathsEqual(target, targetToFilter.intendedParentPath)
      })
    }
  }

  const filteredReparentFactories = factories.filter((reparentStrategy) =>
    targetIsValid(reparentStrategy.targetParent.intendedParentPath),
  )

  return mapDropNulls(({ factory, dragType, targetParent }) => {
    const strategy = factory(canvasState, interactionSession, customStrategyState)
    if (strategy == null) {
      return null
    }
    const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
    const isReparentedWithinComponent = targets.some((target) =>
      EP.pathsEqual(
        EP.getContainingComponent(target),
        EP.getContainingComponent(targetParent.intendedParentPath),
      ),
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
