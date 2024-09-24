import {
  combineElementsToRerender,
  type ElementsToRerender,
} from '../../../../components/editor/store/editor-state'
import {
  getSimpleAttributeAtPath,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { defaultEither, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import { nullIfInfinity, rectanglesEqual } from '../../../../core/shared/math-utils'
import { CSSCursor } from '../../canvas-types'
import type { CanvasCommand } from '../../commands/commands'
import { highlightElementsCommand } from '../../commands/highlight-element-command'
import { setCursorCommand } from '../../commands/set-cursor-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { onlyFitWhenThisControlIsActive, type MetaCanvasStrategy } from '../canvas-strategies'
import type {
  CanvasStrategy,
  InteractionCanvasState,
  InteractionLifecycle,
  StrategyApplicationResult,
} from '../canvas-strategy-types'
import { getTargetPathsFromInteractionTarget, targetPaths } from '../canvas-strategy-types'
import { DoNothingStrategyID } from './drag-to-move-metastrategy'
import { retargetStrategyToChildrenOfFragmentLikeElements } from './fragment-like-helpers'
import type { Optic } from '../../../../core/shared/optics/optics'
import { filtered, fromField } from '../../../../core/shared/optics/optic-creators'
import { modify } from '../../../../core/shared/optics/optic-utilities'

const ANCESTOR_INCOMPATIBLE_STRATEGIES = [DoNothingStrategyID]

export function ancestorMetaStrategy(
  allOtherStrategies: Array<MetaCanvasStrategy>,
  level: number,
): MetaCanvasStrategy {
  return (canvasState, interactionSession, customStrategyState) => {
    // Don't apply during insertion
    if (canvasState.interactionTarget.type === 'INSERTION_SUBJECTS') {
      return []
    }

    const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

    if (targets.length !== 1) {
      return []
    }

    const target = targets[0]

    // Avoid children of the storyboard
    if (EP.isEmptyPath(target) || EP.isStoryboardPath(target) || EP.isStoryboardChild(target)) {
      // TODO Maybe avoid root elements?
      return []
    }

    const unrolledChildren = retargetStrategyToChildrenOfFragmentLikeElements(canvasState).paths

    if (unrolledChildren.length !== 1) {
      return []
    }

    const unrolledChild = unrolledChildren[0]

    // Is the selected element an only child?
    const siblings = MetadataUtils.getSiblingsOrdered(
      canvasState.startingMetadata,
      canvasState.startingElementPathTree,
      unrolledChild,
    )
    if (siblings.length > 1) {
      return []
    }

    // Is the selected element a flow layout element?
    const targetMetadata = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      unrolledChild,
    )
    const isStaticLayout = !(
      MetadataUtils.isPositionAbsolute(targetMetadata) ||
      MetadataUtils.isPositionRelative(targetMetadata) ||
      MetadataUtils.isParentYogaLayoutedContainerForElementAndElementParticipatesInLayout(
        targetMetadata,
      )
    )

    const shouldCheckAncestor = isStaticLayout || EP.isRootElementOfInstance(target)

    if (!shouldCheckAncestor) {
      return []
    }

    const targetFrame = targetMetadata == null ? null : nullIfInfinity(targetMetadata.globalFrame)
    const parentFrame = targetMetadata?.specialSizeMeasurements.immediateParentBounds
    const targetSmallerThatParent =
      targetFrame != null &&
      parentFrame != null &&
      (parentFrame.width > targetFrame.width || parentFrame.height > targetFrame.height)

    if (targetSmallerThatParent) {
      return []
    }

    // TODO Should we also check isParentZeroSized || isParentContiguous?

    // Time to offer up available strategies for the parent
    const parentPath = EP.parentPath(target)
    const ancestorTargetPaths = targetPaths([parentPath])
    const adjustedCanvasState: InteractionCanvasState = {
      ...canvasState,
      interactionTarget: ancestorTargetPaths,
    }

    // Avoid a cyclic dependency by explicitly passing the other metastrategies when creating the next layer's meta strategy
    const nextAncestorResult = ancestorMetaStrategy(allOtherStrategies, level + 1)(
      adjustedCanvasState,
      interactionSession,
      customStrategyState,
    )

    if (nextAncestorResult.length > 0) {
      const fitness = (s: CanvasStrategy) =>
        Math.max(
          onlyFitWhenThisControlIsActive(interactionSession, 'BOUNDING_AREA', s.fitness),
          onlyFitWhenThisControlIsActive(interactionSession, 'KEYBOARD_CATCHER_CONTROL', s.fitness),
        )
      // A length of > 0 means that we should be bubbling up to the next ancestor
      return nextAncestorResult.map((s) => ({
        ...s,
        fitness: fitness(s),
        apply: appendElementsToRerenderToApplyResult(appendCommandsToApplyResult(s.apply, [], []), [
          target,
        ]),
      }))
    } else {
      // Otherwise we should stop at this ancestor and return the strategies for this ancestor
      const fitness = (s: CanvasStrategy) => {
        const value = s.fitness > 0 ? s.fitness + 10 : s.fitness
        return Math.max(
          onlyFitWhenThisControlIsActive(interactionSession, 'BOUNDING_AREA', value),
          onlyFitWhenThisControlIsActive(interactionSession, 'KEYBOARD_CATCHER_CONTROL', value),
        )
      }
      return allOtherStrategies.flatMap((metaStrategy) =>
        metaStrategy(adjustedCanvasState, interactionSession, customStrategyState).flatMap((s) => {
          if (ANCESTOR_INCOMPATIBLE_STRATEGIES.includes(s.id)) {
            return []
          }
          return {
            ...s,
            id: `${s.id}_ANCESTOR_${level}`,
            name: applyLevelSuffix(s.name, level),
            fitness: fitness(s),
            apply: appendElementsToRerenderToApplyResult(
              appendCommandsToApplyResult(
                s.apply,
                [highlightElementsCommand([parentPath]), setCursorCommand(CSSCursor.MovingMagic)],
                [
                  wildcardPatch('mid-interaction', {
                    canvas: {
                      controls: {
                        dragToMoveIndicatorFlags: { ancestor: { $set: true } },
                      },
                    },
                  }),
                ],
              ),
              [target],
            ),
          }
        }),
      )
    }
  }
}

type ApplyFn = CanvasStrategy['apply']

export function appendCommandsToApplyResult(
  applyFn: ApplyFn,
  commandsToAppendToExtendResult: Array<CanvasCommand>,
  commandsToAlwaysAppend: Array<CanvasCommand>,
): ApplyFn {
  return (strategyLifecycle: InteractionLifecycle) => {
    const result = applyFn(strategyLifecycle)
    if (result.status === 'success') {
      if (result.commands.length > 0) {
        return {
          ...result,
          commands: [
            ...result.commands,
            ...commandsToAppendToExtendResult,
            ...commandsToAlwaysAppend,
          ],
        }
      } else {
        return {
          ...result,
          commands: [...result.commands, ...commandsToAlwaysAppend],
        }
      }
    } else {
      return result
    }
  }
}

const strategyResultElementsToRerenderOptic: Optic<StrategyApplicationResult, ElementsToRerender> =
  filtered<StrategyApplicationResult>((result) => result.status === 'success').compose(
    fromField('elementsToRerender'),
  )

export function appendElementsToRerenderToApplyResult(
  applyFn: ApplyFn,
  additionalElementsToRerender: ElementsToRerender,
): ApplyFn {
  return (strategyLifecycle: InteractionLifecycle) => {
    const result = applyFn(strategyLifecycle)
    return modify(
      strategyResultElementsToRerenderOptic,
      (toRerender) => {
        return combineElementsToRerender(toRerender, additionalElementsToRerender)
      },
      result,
    )
  }
}

function applyLevelSuffix(name: string, level: number): string {
  // FIXME What should we use for the label here?
  const newSuffix = `(Up ${level})`
  const oldSuffixIndex = name.indexOf(' (Up')
  const withoutOldSuffix = oldSuffixIndex > 0 ? name.slice(0, oldSuffixIndex) : name
  return `${withoutOldSuffix} ${newSuffix}`
}
