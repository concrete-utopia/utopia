import * as EP from '../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { memoize } from '../../../core/shared/memoize'
import { ElementPath } from '../../../core/shared/project-file-types'
import { AllElementProps } from '../../editor/store/editor-state'
import { getApplicableStrategies, RegisteredCanvasStrategies } from './canvas-strategies'
import {
  CanvasStrategy,
  emptyStrategyApplicationResult,
  InteractionCanvasState,
} from './canvas-strategy-types'
import { InteractionSession } from './interaction-state'

export const lookForApplicableParentStrategy: CanvasStrategy = {
  id: 'LOOK_FOR_APPLICABLE_PARENT_ID',
  name: 'Applicable parent',
  controlsToRender: [],
  isApplicable: (canvasState, interactionSession, metadata, allElementProps) => {
    if (interactionSession == null || interactionSession.interactionData.type !== 'DRAG') {
      return false
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
      return false
    }

    const allStrategies = isApplicableTraverseMemo(
      strategiesMinusTraverse,
      canvasState,
      interactionSession,
      metadata,
      allElementProps,
    )

    return allStrategies.length > 0
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
    const strategiesMinusTraverse = RegisteredCanvasStrategies.filter(
      ({ id }) => id !== 'LOOK_FOR_APPLICABLE_PARENT_ID',
    )

    const applicableStrategies = isApplicableTraverseMemo(
      strategiesMinusTraverse,
      canvasState,
      interactionSession,
      strategyState.startingMetadata,
      strategyState.startingAllElementProps,
    ).sort(
      // TODO: better/idiomatic way of sorting
      (a, b) =>
        a.fitness(canvasState, interactionSession, strategyState) -
        b.fitness(canvasState, interactionSession, strategyState),
    )

    if (applicableStrategies.length > 0) {
      const chosenStrategy = applicableStrategies[0]
      // console.log(chosenStrategy.id)
      return chosenStrategy.apply(canvasState, interactionSession, strategyState)
    }
    return emptyStrategyApplicationResult
  },
}

function* elementAncestry(path: ElementPath) {
  let currentParentPath = path
  while (!EP.isStoryboardChild(currentParentPath)) {
    yield currentParentPath
    const parent = EP.parentPath(currentParentPath)
    currentParentPath = parent
  }
}

function isSingletonAbsoluteMove(strategies: Array<CanvasStrategy>): boolean {
  return strategies.length === 1 && strategies[0].id === 'CONVERT_TO_ABSOLUTE_AND_MOVE_STRATEGY'
}

function patchCanvasStateInteractionTargetPath(
  canvasState: InteractionCanvasState,
  path: ElementPath,
): InteractionCanvasState {
  return {
    ...canvasState,
    interactionTarget: {
      type: 'TARGET_PATHS',
      elements: [path],
    },
  }
}

function isApplicableTraverse(
  strategies: Array<CanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): Array<CanvasStrategy> {
  if (
    canvasState.interactionTarget.type !== 'TARGET_PATHS' ||
    canvasState.interactionTarget.elements.length !== 1
  ) {
    return getApplicableStrategies(
      strategies,
      canvasState,
      interactionSession,
      metadata,
      allElementProps,
    )
  }

  for (const path of elementAncestry(canvasState.interactionTarget.elements[0])) {
    const patchedCanvasState = patchCanvasStateInteractionTargetPath(canvasState, path)
    const applicableStrategies = getApplicableStrategies(
      strategies,
      patchedCanvasState,
      interactionSession,
      metadata,
      allElementProps,
    )

    if (!isSingletonAbsoluteMove(applicableStrategies)) {
      return applicableStrategies
    }
  }

  return []
}

const isApplicableTraverseMemo = memoize(isApplicableTraverse)
