import React from 'react'
import { createSelector } from 'reselect'
import { addAllUniquelyBy, mapDropNulls, sortBy } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { arrayEquals } from '../../../core/shared/utils'
import { InteractionSession, SessionStateState } from './interaction-state'
import { InnerDispatchResult } from '../../editor/store/dispatch'
import { EditorStorePatched } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasCommand } from '../commands/commands'
import { CanvasStrategy, ControlWithKey, InteractionCanvasState } from './canvas-strategy-types'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = []

function getApplicableStrategies(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession | null,
  metadata: ElementInstanceMetadataMap,
): Array<CanvasStrategy> {
  return RegisteredCanvasStrategies.filter((strategy) => {
    return strategy.isApplicable(canvasState, interactionState, metadata)
  })
}

const getApplicableStrategiesSelector = createSelector(
  (store: EditorStorePatched): InteractionCanvasState => {
    return {
      selectedElements: store.editor.selectedViews,
      // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
      projectContents: store.editor.projectContents,
      openFile: store.editor.canvas.openFile?.filename,
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  },
  (store: EditorStorePatched) => store.editor.canvas.interactionSession,
  (store: EditorStorePatched) => store.editor.jsxMetadata,
  (
    canvasState: InteractionCanvasState,
    interactionState: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
  ): Array<CanvasStrategy> => {
    return getApplicableStrategies(canvasState, interactionState, metadata)
  },
)

function useGetApplicableStrategies(): Array<CanvasStrategy> {
  return useEditorState(getApplicableStrategiesSelector, 'useGetApplicableStrategies', arrayEquals)
}

interface StrategyWithFitness {
  fitness: number
  strategy: CanvasStrategy
}

function getApplicableStrategiesOrderedByFitness(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  sessionState: SessionStateState,
): Array<StrategyWithFitness> {
  const applicableStrategies = getApplicableStrategies(
    canvasState,
    interactionState,
    sessionState.startingMetadata,
  )

  // Compute the fitness results upfront.
  const strategiesWithFitness = mapDropNulls((strategy) => {
    const fitness = strategy.fitness(canvasState, interactionState, sessionState)
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
    return {
      selectedElements: store.editor.selectedViews,
      // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
      projectContents: store.editor.projectContents,
      openFile: store.editor.canvas.openFile?.filename,
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  },
  (store: EditorStorePatched) => store.editor.canvas.interactionSession,
  (store: EditorStorePatched) => store.sessionStateState,
  (
    canvasState: InteractionCanvasState,
    interactionState: InteractionSession | null,
    sessionState: SessionStateState,
  ): Array<string> => {
    if (interactionState == null) {
      return []
    }
    return getApplicableStrategiesOrderedByFitness(canvasState, interactionState, sessionState).map(
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

function pickDefaultCanvasStrategy(
  sortedApplicableStrategies: Array<StrategyWithFitness>,
  previousStrategyName: string | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  const currentBestStrategy = sortedApplicableStrategies[0] ?? null
  const previousStrategy =
    sortedApplicableStrategies.find((s) => s.strategy.name === previousStrategyName) ?? null
  if (previousStrategy != null && previousStrategy.fitness === currentBestStrategy.fitness) {
    return { strategy: previousStrategy, previousStrategy: previousStrategy }
  } else {
    return { strategy: currentBestStrategy, previousStrategy: previousStrategy }
  }
}

function pickStrategy(
  sortedApplicableStrategies: Array<StrategyWithFitness>,
  interactionState: InteractionSession,
  previousStrategyName: string | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  // FIXME Explicitly picking a strategy will prevent natural handovers that otherwise should occur

  if (interactionState.userPreferredStrategy != null) {
    const foundStrategyByName = sortedApplicableStrategies.find(
      (s) => s.strategy.name === interactionState.userPreferredStrategy,
    )
    const foundPreviousStrategy =
      sortedApplicableStrategies.find((s) => s.strategy.name === previousStrategyName) ?? null

    if (foundStrategyByName != null) {
      return { strategy: foundStrategyByName, previousStrategy: foundPreviousStrategy }
    }
  }
  // fall back to default strategy
  return pickDefaultCanvasStrategy(sortedApplicableStrategies, previousStrategyName)
}

export function findCanvasStrategy(
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  sessionState: SessionStateState,
  previousStrategyName: string | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  const sortedApplicableStrategies = getApplicableStrategiesOrderedByFitness(
    canvasState,
    interactionState,
    sessionState,
  )
  return pickStrategy(sortedApplicableStrategies, interactionState, previousStrategyName)
}

export function applyCanvasStrategy(
  strategy: CanvasStrategy,
  canvasState: InteractionCanvasState,
  interactionState: InteractionSession,
  sessionState: SessionStateState,
): Array<CanvasCommand> {
  return strategy.apply(canvasState, interactionState, sessionState)
}

export function useGetApplicableStrategyControls(): Array<ControlWithKey> {
  const applicableStrategies = useGetApplicableStrategies()
  const currentStrategy = useEditorState(
    (store) => store.sessionStateState.currentStrategy,
    'currentStrategy',
  )
  return React.useMemo(() => {
    return applicableStrategies.reduce<ControlWithKey[]>((working, s) => {
      const filteredControls = s.controlsToRender.filter(
        (control) =>
          control.show === 'always-visible' ||
          (control.show === 'visible-only-while-active' && s.name === currentStrategy),
      )
      return addAllUniquelyBy(working, filteredControls, (l, r) => l.control === r.control)
    }, [])
  }, [applicableStrategies, currentStrategy])
}

export function findCanvasStrategyFromDispatchResult(
  result: InnerDispatchResult,
): StrategyWithFitness | null {
  const newEditorState = result.unpatchedEditor
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
  const interactionState = newEditorState.canvas.interactionSession
  if (interactionState == null) {
    return null
  } else {
    const { strategy } = findCanvasStrategy(
      canvasState,
      interactionState,
      result.sessionStateState,
      result.sessionStateState.currentStrategy,
    )
    return strategy
  }
}

export function isStrategyActive(sessionState: SessionStateState): boolean {
  return (
    sessionState.accumulatedCommands.length > 0 || sessionState.currentStrategyCommands.length > 0
  )
}
