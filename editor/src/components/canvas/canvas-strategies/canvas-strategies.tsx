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
  interactionSession: InteractionSession | null,
  metadata: ElementInstanceMetadataMap,
): Array<CanvasStrategy> {
  return RegisteredCanvasStrategies.filter((strategy) => {
    return strategy.isApplicable(canvasState, interactionSession, metadata)
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
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
  ): Array<CanvasStrategy> => {
    return getApplicableStrategies(canvasState, interactionSession, metadata)
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
  interactionSession: InteractionSession,
  sessionState: SessionStateState,
): Array<StrategyWithFitness> {
  const applicableStrategies = getApplicableStrategies(
    canvasState,
    interactionSession,
    sessionState.startingMetadata,
  )

  // Compute the fitness results upfront.
  const strategiesWithFitness = mapDropNulls((strategy) => {
    const fitness = strategy.fitness(canvasState, interactionSession, sessionState)
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
    interactionSession: InteractionSession | null,
    sessionState: SessionStateState,
  ): Array<string> => {
    if (interactionSession == null) {
      return []
    }
    return getApplicableStrategiesOrderedByFitness(
      canvasState,
      interactionSession,
      sessionState,
    ).map((s) => s.strategy.name)
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
  interactionSession: InteractionSession,
  previousStrategyName: string | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  // FIXME Explicitly picking a strategy will prevent natural handovers that otherwise should occur

  if (interactionSession.userPreferredStrategy != null) {
    const foundStrategyByName = sortedApplicableStrategies.find(
      (s) => s.strategy.name === interactionSession.userPreferredStrategy,
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
  interactionSession: InteractionSession,
  sessionState: SessionStateState,
  previousStrategyName: string | null,
): { strategy: StrategyWithFitness | null; previousStrategy: StrategyWithFitness | null } {
  const sortedApplicableStrategies = getApplicableStrategiesOrderedByFitness(
    canvasState,
    interactionSession,
    sessionState,
  )
  return pickStrategy(sortedApplicableStrategies, interactionSession, previousStrategyName)
}

export function applyCanvasStrategy(
  strategy: CanvasStrategy,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  sessionState: SessionStateState,
): Array<CanvasCommand> {
  return strategy.apply(canvasState, interactionSession, sessionState)
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
  const interactionSession = newEditorState.canvas.interactionSession
  if (interactionSession == null) {
    return null
  } else {
    const { strategy } = findCanvasStrategy(
      canvasState,
      interactionSession,
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
