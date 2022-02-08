import React from 'react'
import { createSelector } from 'reselect'
import { intersects } from 'semver'
import { addAllUniquelyBy, mapDropNulls, sortBy } from '../../../core/shared/array-utils'
import { offsetPoint, vectorDifference } from '../../../core/shared/math-utils'
import { arrayEquals } from '../../../core/shared/utils'
import {
  CanvasState,
  CanvasStrategy,
  ControlWithKey,
  DragInteractionData,
  InteractionData,
  InteractionState,
  SessionStateState,
} from '../../../interactions_proposal'
import { EditorStore } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasCommand } from '../commands/commands'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import { ancestorAbsoluteMoveStrategy } from './ancestor-absolute-move-strategy'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'
import { flexBasisResizeStrategy, flexGrowResizeStrategy } from './flex-basis-resize-strategy'
import { flexGapStrategy } from './flex-gap-strategy'
import { flexReOrderStrategy } from './flex-reorder-strategy'
import { parentPaddingAdjustStrategy } from './parent-padding-adjust-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [
  flexBasisResizeStrategy,
  flexGrowResizeStrategy,
  flexGapStrategy,
  flexReOrderStrategy,
  flexAlignParentStrategy,
  parentPaddingAdjustStrategy,
  ancestorAbsoluteMoveStrategy,
  absoluteMoveStrategy,
  absoluteReparentStrategy,
]

export function getStrategyByName(name: string): CanvasStrategy | null {
  return (
    RegisteredCanvasStrategies.find((s) => {
      return s.name === name
    }) ?? null
  )
}

export function strategiesPartOfSameGroup(
  oldStrategyName: string | null,
  newStrategyName: string | null,
): boolean {
  if (oldStrategyName == null || newStrategyName == null || oldStrategyName === newStrategyName) {
    return false
  } else {
    const oldStrategy = getStrategyByName(oldStrategyName)
    const newStrategy = getStrategyByName(newStrategyName)
    if (oldStrategy == null || newStrategy == null) {
      return false
    } else {
      for (const key of oldStrategy.strategyGroups) {
        if (newStrategy.strategyGroups.has(key)) {
          return true
        }
      }
      return false
    }
  }
}

function getApplicableStrategies(
  canvasState: CanvasState,
  interactionState: InteractionState | null,
): Array<CanvasStrategy> {
  return RegisteredCanvasStrategies.filter((strategy) => {
    return strategy.isApplicable(canvasState, interactionState)
  })
}

const getApplicableStrategiesSelector = createSelector(
  (store: EditorStore): CanvasState => {
    return {
      selectedElements: store.editor.selectedViews,
      metadata: store.editor.jsxMetadata,
      projectContents: store.editor.projectContents,
      openFile: store.editor.canvas.openFile?.filename,
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  },
  (store: EditorStore) => store.editor.canvas.interactionState,
  (canvasState: CanvasState, interactionState: InteractionState | null): Array<CanvasStrategy> => {
    return getApplicableStrategies(canvasState, interactionState)
  },
)

function useGetApplicableStrategies(): Array<CanvasStrategy> {
  return useEditorState(getApplicableStrategiesSelector, 'useGetApplicableStrategies', arrayEquals)
}

interface StrategiesWithFitness {
  fitness: number
  strategy: CanvasStrategy
}

function getApplicableStrategiesOrderedByFitness(
  canvasState: CanvasState,
  interactionState: InteractionState,
  sessionState: SessionStateState,
): Array<StrategiesWithFitness> {
  const applicableStrategies = getApplicableStrategies(canvasState, interactionState)

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
  (store: EditorStore): CanvasState => {
    return {
      selectedElements: store.editor.selectedViews,
      metadata: store.editor.jsxMetadata,
      projectContents: store.editor.projectContents,
      openFile: store.editor.canvas.openFile?.filename,
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  },
  (store: EditorStore) => store.editor.canvas.interactionState,
  (store: EditorStore) => store.sessionStateState,
  (
    canvasState: CanvasState,
    interactionState: InteractionState | null,
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

export function findCanvasStrategy(
  canvasState: CanvasState,
  interactionState: InteractionState,
  sessionState: SessionStateState,
): CanvasStrategy | null {
  const applicableStrategies = getApplicableStrategiesOrderedByFitness(
    canvasState,
    interactionState,
    sessionState,
  )
  return pickStrategy(applicableStrategies, interactionState)
}

export function applyCanvasStrategy(
  strategy: CanvasStrategy,
  canvasState: CanvasState,
  interactionState: InteractionState,
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

export function strategySwitchInteractionDataReset(
  interactionData: InteractionData,
): InteractionData {
  switch (interactionData.type) {
    case 'DRAG':
      if (interactionData.drag == null) {
        return interactionData
      } else {
        return {
          ...interactionData,
          dragStart: offsetPoint(interactionData.dragStart, interactionData.drag),
          drag: null,
        }
      }
    case 'KEYBOARD':
      return interactionData
    default:
      const _exhaustiveCheck: never = interactionData
      throw new Error(`Unhandled interaction type ${JSON.stringify(interactionData)}`)
  }
}

export function strategySwitchInteractionStateReset(
  interactionState: InteractionState,
): InteractionState {
  return {
    ...interactionState,
    interactionData: strategySwitchInteractionDataReset(interactionState.interactionData),
  }
}
