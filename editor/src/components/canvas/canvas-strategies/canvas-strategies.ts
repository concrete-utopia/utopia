import React from 'react'
import { createSelector } from 'reselect'
import { addAllUniquelyBy, sortBy } from '../../../core/shared/array-utils'
import {
  CanvasState,
  CanvasStrategy,
  ControlWithKey,
  InteractionState,
  SessionStateState,
} from '../../../interactions_proposal'
import { EditorStore } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasCommand } from '../commands/commands'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'
import { flexGapStrategy } from './flex-gap-strategy'
import { parentPaddingAdjustStrategy } from './parent-padding-adjust-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [
  flexGapStrategy,
  flexAlignParentStrategy,
  parentPaddingAdjustStrategy,
]

interface StrategiesWithFitness {
  fitness: number
  strategy: CanvasStrategy
}

const getApplicableStrategiesOrderedByFitnessSelector = createSelector(
  (store: EditorStore): CanvasState => {
    return {
      selectedElements: store.editor.selectedViews,
      metadata: store.editor.jsxMetadata,
      projectContents: store.editor.projectContents,
      openFile: store.editor.canvas.openFile?.filename,
    }
  },
  (store: EditorStore) => store.editor.canvas.interactionState,
  (canvasState: CanvasState, interactionState: InteractionState | null): Array<string> => {
    if (interactionState == null) {
      return []
    }
    return getApplicableStrategiesOrderedByFitness(canvasState, interactionState).map(
      (s) => s.strategy.name,
    )
  },
)

export function useGetApplicableStrategiesOrderedByFitness(): Array<string> {
  return useEditorState(
    getApplicableStrategiesOrderedByFitnessSelector,
    'useGetApplicableStrategiesOrderedByFitness',
  )
}

function getApplicableStrategies(
  canvasState: CanvasState,
  interactionState: InteractionState | null,
): Array<CanvasStrategy> {
  return RegisteredCanvasStrategies.filter((strategy) => {
    return strategy.isApplicable(canvasState, interactionState)
  })
}

function getApplicableStrategiesOrderedByFitness(
  canvasState: CanvasState,
  interactionState: InteractionState,
): Array<StrategiesWithFitness> {
  const applicableStrategies = getApplicableStrategies(canvasState, interactionState)

  // Compute the fitness results upfront.
  const strategiesWithFitness = applicableStrategies.map((strategy) => {
    return {
      fitness: strategy.fitness(canvasState, interactionState),
      strategy: strategy,
    }
  })

  return sortBy(strategiesWithFitness, (l, r) => {
    // sort by fitness, descending
    return r.fitness - l.fitness
  })
}

export function pickDefaultCanvasStrategy(
  applicableStrategies: Array<StrategiesWithFitness>,
): CanvasStrategy | null {
  return applicableStrategies[0]?.strategy ?? null
}

function pickStrategy(
  applicableStrategies: Array<StrategiesWithFitness>,
  interactionState: InteractionState,
): CanvasStrategy | null {
  // FIXME Explicitly picking a strategy will prevent natural handovers that otherwise should occur

  if (interactionState.userPreferredStrategy != null) {
    const foundStrategyByName = applicableStrategies.find(
      (s) => s.strategy.name === interactionState.userPreferredStrategy,
    )
    if (foundStrategyByName != null) {
      return foundStrategyByName.strategy
    }
  }
  // fall back to default strategy
  return pickDefaultCanvasStrategy(applicableStrategies)
}

export function applyCanvasStrategy(
  canvasState: CanvasState,
  interactionState: InteractionState,
  sessionState: SessionStateState,
): { commands: Array<CanvasCommand>; strategy: string | null } {
  const applicableStrategies = getApplicableStrategiesOrderedByFitness(
    canvasState,
    interactionState,
  )
  const strategy = pickStrategy(applicableStrategies, interactionState)
  if (strategy == null) {
    return {
      commands: [],
      strategy: null,
    }
  } else {
    const commands = strategy.apply(canvasState, interactionState, sessionState)
    return {
      commands: commands,
      strategy: strategy.name,
    }
  }
}

const getStrategyControlsSelector = createSelector(
  (store: EditorStore): CanvasState => {
    return {
      selectedElements: store.editor.selectedViews,
      metadata: store.editor.jsxMetadata,
      projectContents: store.editor.projectContents,
      openFile: store.editor.canvas.openFile?.filename,
    }
  },
  (store: EditorStore) => store.editor.canvas.interactionState,
  (canvasState: CanvasState, interactionState: InteractionState | null): Array<ControlWithKey> => {
    const applicableStrategiesWithFitness = getApplicableStrategies(canvasState, interactionState)
    return applicableStrategiesWithFitness.reduce((working, s) => {
      // FIXME This part needs memoising
      return addAllUniquelyBy(working, s.controlsToRender, (l, r) => l.control === r.control)
    }, [] as Array<ControlWithKey>)
  },
)

export function useGetApplicableStrategyControls(): Array<ControlWithKey> {
  return useEditorState(getStrategyControlsSelector, 'useGetApplicableStrategyControls')
}
