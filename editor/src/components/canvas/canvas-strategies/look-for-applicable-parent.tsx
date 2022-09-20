import { sortBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { memoize } from '../../../core/shared/memoize'
import { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import { AllElementProps } from '../../editor/store/editor-state'
import { CSSCursor } from '../canvas-types'
import { highlightElementsCommand } from '../commands/highlight-element-command'
import { setCursorCommand } from '../commands/set-cursor-command'
import { updateSelectedViews } from '../commands/update-selected-views-command'
import { ParentBounds } from '../controls/parent-bounds'
import { ParentOutlines } from '../controls/parent-outlines'
import { DragOutlineControl } from '../controls/select-mode/drag-outline-control'
import {
  calculateStrategiesWithFitness,
  getApplicableStrategies,
  RegisteredCanvasStrategies,
} from './canvas-strategies'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
  InteractionTarget,
  strategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'

export const lookForApplicableParentStrategy: CanvasStrategy = {
  id: 'LOOK_FOR_APPLICABLE_PARENT_ID',
  name: (canvasState, interactionSession, strategyState) => {
    const defaultName = 'Applicable parent'
    if (interactionSession == null) {
      return defaultName
    }

    const result = isApplicableInner(
      canvasState,
      interactionSession,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )

    if (result == null || result.strategies.length === 0) {
      return defaultName
    }

    const strategiesWithFitness = calculateStrategiesWithFitness(
      result.strategies,
      canvasState,
      interactionSession,
      strategyState,
    )

    const sortedStrategies = sortBy(strategiesWithFitness, (l, r) => {
      // sort by fitness, descending
      return r.fitness - l.fitness
    })

    const fittestStrategy = sortedStrategies[0]

    return fittestStrategy.name(canvasState, interactionSession, strategyState) + '*'
  },
  controlsToRender: [
    {
      control: DragOutlineControl,
      key: 'ghost-outline-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentOutlines,
      key: 'parent-outlines-control',
      show: 'visible-only-while-active',
    },
    {
      control: ParentBounds,
      key: 'parent-bounds-control',
      show: 'visible-only-while-active',
    },
  ],
  isApplicable: (canvasState, interactionSession, metadata, allElementProps) => {
    if (interactionSession == null) {
      return false
    }

    const strategies = isApplicableInner(canvasState, interactionSession, metadata, allElementProps)
    return strategies != null
  },

  fitness: (canvasState, interactionSession, strategyState) => {
    return lookForApplicableParentStrategy.isApplicable(
      canvasState,
      interactionSession,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )
      ? 1
      : 0
  },

  apply: (canvasState, interactionSession, strategyState) => {
    const result = isApplicableInner(
      canvasState,
      interactionSession,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    )

    if (result == null) {
      return emptyStrategyApplicationResult
    }

    const { strategies, effectiveTarget, originalTarget } = result
    if (strategies.length < 1) {
      return emptyStrategyApplicationResult
    }

    const strategiesInFitnessOrder = strategies.sort(
      // TODO: better/idiomatic way of sorting
      (a, b) =>
        a.fitness(canvasState, interactionSession, strategyState) -
        b.fitness(canvasState, interactionSession, strategyState),
    )

    const chosenStrategy = strategiesInFitnessOrder[0]
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(canvasState, effectiveTarget)

    const chosenStrategyApplicationResult = chosenStrategy.apply(
      patchedCanvasState,
      interactionSession,
      strategyState,
    )

    return strategyApplicationResult(
      [
        ...chosenStrategyApplicationResult.commands,
        highlightElementsCommand(effectiveTarget),
        updateSelectedViews('mid-interaction', originalTarget),
        setCursorCommand('mid-interaction', CSSCursor.MovingMagic),
      ],
      chosenStrategyApplicationResult.customStatePatch,
      chosenStrategyApplicationResult.status,
    )
  },
}

function* elementAncestry(path: ElementPath) {
  let currentParentPath = path
  const componentsInSubtree: Array<ElementPath> = [path]
  while (!EP.isStoryboardChild(currentParentPath)) {
    yield {
      root: currentParentPath,
      componentsInSubtree,
    }
    const parent = EP.parentPath(currentParentPath)
    componentsInSubtree.push(parent)
    currentParentPath = parent
  }
}

function isSingletonAbsoluteMove(strategies: Array<CanvasStrategy>): boolean {
  return strategies.length === 1 && strategies[0].id === 'CONVERT_TO_ABSOLUTE_AND_MOVE_STRATEGY'
}

function patchCanvasStateInteractionTargetPath(
  canvasState: InteractionCanvasState,
  path: ElementPath[],
): InteractionCanvasState {
  return {
    ...canvasState,
    interactionTarget: {
      type: 'TARGET_PATHS',
      elements: path,
    },
  }
}

function isApplicableInner(
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): IsApplicableTraverseResult | null {
  if (interactionSession.interactionData.type !== 'DRAG') {
    return null
  }

  const strategiesMinusTraverse = RegisteredCanvasStrategies.filter(
    ({ id }) => id !== 'LOOK_FOR_APPLICABLE_PARENT_ID',
  )

  const applicableStrategies = getApplicableStrategies(
    strategiesMinusTraverse,
    canvasState,
    interactionSession,
    metadata,
    allElementProps,
  )

  if (!isSingletonAbsoluteMove(applicableStrategies)) {
    return null
  }
  const result = isApplicableTraverseMemo(
    strategiesMinusTraverse,
    canvasState,
    interactionSession,
    metadata,
    allElementProps,
  )

  if (result == null || result.strategies.length < 1) {
    return null
  }

  return result
}

interface IsApplicableTraverseResult {
  strategies: Array<CanvasStrategy>
  effectiveTarget: Array<ElementPath>
  originalTarget: Array<ElementPath>
  componentsInSubtree: Array<ElementPath>
}

function isApplicableTraverse(
  strategies: Array<CanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): IsApplicableTraverseResult | null {
  if (
    canvasState.interactionTarget.type !== 'TARGET_PATHS' ||
    canvasState.interactionTarget.elements.length !== 1
  ) {
    const applicableStrategies = getApplicableStrategies(
      strategies,
      canvasState,
      interactionSession,
      metadata,
      allElementProps,
    )
    return {
      strategies: applicableStrategies,
      effectiveTarget: pathsFromInteractionTarget(canvasState.interactionTarget),
      originalTarget: pathsFromInteractionTarget(canvasState.interactionTarget),
      componentsInSubtree: [],
    }
  }

  for (const { root: path, componentsInSubtree } of elementAncestry(
    canvasState.interactionTarget.elements[0],
  )) {
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(canvasState, [path])
    const applicableStrategies = getApplicableStrategies(
      strategies,
      patchedCanvasState,
      interactionSession,
      metadata,
      allElementProps,
    )

    if (!isSingletonAbsoluteMove(applicableStrategies)) {
      return {
        strategies: applicableStrategies,
        effectiveTarget: [path],
        originalTarget: canvasState.interactionTarget.elements,
        componentsInSubtree,
      }
    }
  }

  return null
}

function pathsFromInteractionTarget(interactionTarget: InteractionTarget): Array<ElementPath> {
  switch (interactionTarget.type) {
    case 'TARGET_PATHS':
      return interactionTarget.elements
    case 'INSERTION_SUBJECTS':
      return []
    default:
      assertNever(interactionTarget)
  }
}

const isApplicableTraverseMemo = memoize(isApplicableTraverse)
