import React from 'react'
import { createSelector } from 'reselect'
import { addAllUniquelyBy, mapDropNulls, sortBy } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { arrayEquals } from '../../../core/shared/utils'
import { InnerDispatchResult } from '../../editor/store/dispatch'
import { AllElementProps, EditorState, EditorStorePatched } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasCommand } from '../commands/commands'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import {
  CanvasStrategy,
  CanvasStrategyId,
  ControlWithKey,
  InteractionCanvasState,
  StrategyApplicationResult,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { keyboardAbsoluteMoveStrategy } from './keyboard-absolute-move-strategy'
import { absoluteResizeBoundingBoxStrategy } from './absolute-resize-bounding-box-strategy'
import { keyboardAbsoluteResizeStrategy } from './keyboard-absolute-resize-strategy'
import { escapeHatchStrategy } from './escape-hatch-strategy'
import { flexReorderStrategy } from './flex-reorder-strategy'
import { absoluteDuplicateStrategy } from './absolute-duplicate-strategy'
import { absoluteReparentToFlexStrategy } from './absolute-reparent-to-flex-strategy'
import { flexReparentToAbsoluteStrategy } from './flex-reparent-to-absolute-strategy'
import { flexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'

export const RegisteredCanvasStrategies: Array<CanvasStrategy> = [
  absoluteMoveStrategy,
  absoluteReparentStrategy,
  absoluteDuplicateStrategy,
  keyboardAbsoluteMoveStrategy,
  keyboardAbsoluteResizeStrategy,
  absoluteResizeBoundingBoxStrategy,
  flexReorderStrategy,
  flexReparentToAbsoluteStrategy,
  flexReparentToFlexStrategy,
  // escapeHatchStrategy,  // TODO re-enable once reparent is not tied to cmd
  absoluteReparentToFlexStrategy,
]

export function pickCanvasStateFromEditorState(
  editorState: EditorState,
  builtInDependencies: BuiltInDependencies,
): InteractionCanvasState {
  return {
    builtInDependencies: builtInDependencies,
    selectedElements: editorState.selectedViews,
    projectContents: editorState.projectContents,
    nodeModules: editorState.nodeModules.files,
    openFile: editorState.canvas.openFile?.filename,
    scale: editorState.canvas.scale,
    canvasOffset: editorState.canvas.roundedCanvasOffset,
  }
}

function getApplicableStrategies(
  strategies: Array<CanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): Array<CanvasStrategy> {
  return strategies.filter((strategy) => {
    return strategy.isApplicable(canvasState, interactionSession, metadata, allElementProps)
  })
}

const getApplicableStrategiesSelector = createSelector(
  (store: EditorStorePatched): InteractionCanvasState => {
    return pickCanvasStateFromEditorState(store.editor, store.builtInDependencies)
  },
  (store: EditorStorePatched) => store.editor.canvas.interactionSession,
  (store: EditorStorePatched) => store.editor.jsxMetadata,
  (store: EditorStorePatched) => store.editor.allElementProps,
  (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
  ): Array<CanvasStrategy> => {
    return getApplicableStrategies(
      RegisteredCanvasStrategies,
      canvasState,
      interactionSession,
      metadata,
      allElementProps,
    )
  },
)

function useGetApplicableStrategies(): Array<CanvasStrategy> {
  return useEditorState(getApplicableStrategiesSelector, 'useGetApplicableStrategies', arrayEquals)
}

export interface StrategyWithFitness {
  fitness: number
  strategy: CanvasStrategy
}

function getApplicableStrategiesOrderedByFitness(
  strategies: Array<CanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
): Array<StrategyWithFitness> {
  const applicableStrategies = getApplicableStrategies(
    strategies,
    canvasState,
    interactionSession,
    strategyState.startingMetadata,
    strategyState.startingAllElementProps,
  )

  // Compute the fitness results upfront.
  const strategiesWithFitness = mapDropNulls((strategy) => {
    const fitness = strategy.fitness(canvasState, interactionSession, strategyState)
    if (fitness <= 0) {
      return null
    } else {
      return {
        fitness: fitness,
        strategy: strategy,
      }
    }
  }, applicableStrategies)

  const sortedStrategies = sortBy(strategiesWithFitness, (l, r) => {
    // sort by fitness, descending
    return r.fitness - l.fitness
  })

  return sortedStrategies
}

const getApplicableStrategiesOrderedByFitnessSelector = createSelector(
  (store: EditorStorePatched): InteractionCanvasState => {
    return pickCanvasStateFromEditorState(store.editor, store.builtInDependencies)
  },
  (store: EditorStorePatched) => store.editor.canvas.interactionSession,
  (store: EditorStorePatched) => store.strategyState,
  (
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    strategyState: StrategyState,
  ): Array<CanvasStrategy> => {
    if (interactionSession == null) {
      return []
    }
    return getApplicableStrategiesOrderedByFitness(
      RegisteredCanvasStrategies,
      canvasState,
      interactionSession,
      strategyState,
    ).map((s) => s.strategy)
  },
)

export function useGetApplicableStrategiesOrderedByFitness(): Array<CanvasStrategy> {
  return useEditorState(
    getApplicableStrategiesOrderedByFitnessSelector,
    'useGetApplicableStrategiesOrderedByFitness',
  )
}

function pickDefaultCanvasStrategy(
  sortedApplicableStrategies: Array<StrategyWithFitness>,
  previousStrategyId: string | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  const currentBestStrategy = sortedApplicableStrategies[0] ?? null
  const previousStrategy =
    sortedApplicableStrategies.find((s) => s.strategy.id === previousStrategyId) ?? null
  if (previousStrategy != null && previousStrategy.fitness === currentBestStrategy.fitness) {
    return { strategy: previousStrategy, previousStrategy: previousStrategy }
  } else {
    return { strategy: currentBestStrategy, previousStrategy: previousStrategy }
  }
}

function pickStrategy(
  sortedApplicableStrategies: Array<StrategyWithFitness>,
  interactionSession: InteractionSession,
  previousStrategyId: CanvasStrategyId | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  // FIXME Explicitly picking a strategy will prevent natural handovers that otherwise should occur

  if (interactionSession.userPreferredStrategy != null) {
    const foundStrategyByName = sortedApplicableStrategies.find(
      (s) => s.strategy.id === interactionSession.userPreferredStrategy,
    )
    const foundPreviousStrategy =
      sortedApplicableStrategies.find((s) => s.strategy.id === previousStrategyId) ?? null

    if (foundStrategyByName != null) {
      return { strategy: foundStrategyByName, previousStrategy: foundPreviousStrategy }
    }
  }
  // fall back to default strategy
  return pickDefaultCanvasStrategy(sortedApplicableStrategies, previousStrategyId)
}

export function findCanvasStrategy(
  strategies: Array<CanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
  previousStrategyId: CanvasStrategyId | null,
): {
  strategy: StrategyWithFitness | null
  previousStrategy: StrategyWithFitness | null
  sortedApplicableStrategies: Array<CanvasStrategy>
} {
  const sortedApplicableStrategies = getApplicableStrategiesOrderedByFitness(
    strategies,
    canvasState,
    interactionSession,
    strategyState,
  )
  return {
    ...pickStrategy(sortedApplicableStrategies, interactionSession, previousStrategyId),
    sortedApplicableStrategies: sortedApplicableStrategies.map((s) => s.strategy),
  }
}

export function applyCanvasStrategy(
  strategy: CanvasStrategy,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
): StrategyApplicationResult {
  return strategy.apply(canvasState, interactionSession, strategyState)
}

export function useGetApplicableStrategyControls(): Array<ControlWithKey> {
  const applicableStrategies = useGetApplicableStrategies()
  const currentStrategy = useEditorState(
    (store) => store.strategyState.currentStrategy,
    'currentStrategy',
  )
  return React.useMemo(() => {
    return applicableStrategies.reduce<ControlWithKey[]>((working, s) => {
      const filteredControls = s.controlsToRender.filter(
        (control) =>
          control.show === 'always-visible' ||
          (control.show === 'visible-only-while-active' && s.id === currentStrategy),
      )
      return addAllUniquelyBy(working, filteredControls, (l, r) => l.control === r.control)
    }, [])
  }, [applicableStrategies, currentStrategy])
}

export function isStrategyActive(strategyState: StrategyState): boolean {
  return (
    strategyState.accumulatedPatches.length > 0 || strategyState.currentStrategyCommands.length > 0
  )
}
