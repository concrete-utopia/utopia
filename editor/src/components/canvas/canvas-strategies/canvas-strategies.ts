import React from 'react'
import { createSelector } from 'reselect'
import { intersects } from 'semver'
import { addAllUniquelyBy, mapDropNulls, sortBy } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { offsetPoint, pointDifference, zeroCanvasPoint } from '../../../core/shared/math-utils'
import { arrayEquals } from '../../../core/shared/utils'
import {
  CanvasStrategy,
  ControlWithKey,
  InteractionCanvasState,
  InteractionData,
  InteractionState,
  SessionStateState,
} from '../../../interactions_proposal'
import { DispatchResult, InnerDispatchResult } from '../../editor/store/dispatch'
import { EditorStore } from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { CanvasCommand } from '../commands/commands'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import { alignToParentStrategy } from './align-to-parent-strategy'
import { ancestorAbsoluteMoveStrategy } from './ancestor-absolute-move-strategy'
import { flexAlignParentStrategy } from './flex-align-parent-strategy'
import { flexBasisResizeStrategy, flexGrowResizeStrategy } from './flex-basis-resize-strategy'
import { flexGapStrategy } from './flex-gap-strategy'
import { flexReOrderStrategy } from './flex-reorder-strategy'
import { flowReOrderStrategy } from './flow-reorder-strategy'
import { adjustMinMaxDimensionStrategy } from './min-max-dimension-adjust-strategy'
import { parentPaddingAdjustStrategy } from './parent-padding-adjust-strategy'

const RegisteredCanvasStrategies: Array<CanvasStrategy> = [
  alignToParentStrategy,
  adjustMinMaxDimensionStrategy,
  flexBasisResizeStrategy,
  flexGrowResizeStrategy,
  flexGapStrategy,
  flexReOrderStrategy,
  flexAlignParentStrategy,
  parentPaddingAdjustStrategy,
  ancestorAbsoluteMoveStrategy,
  absoluteMoveStrategy,
  absoluteReparentStrategy,
  flowReOrderStrategy,
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
  canvasState: InteractionCanvasState,
  interactionState: InteractionState | null,
  metadata: ElementInstanceMetadataMap,
): Array<CanvasStrategy> {
  return RegisteredCanvasStrategies.filter((strategy) => {
    return strategy.isApplicable(canvasState, interactionState, metadata)
  })
}

const getApplicableStrategiesSelector = createSelector(
  (store: EditorStore): InteractionCanvasState => {
    return {
      selectedElements: store.editor.selectedViews,
      // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
      projectContents: store.editor.projectContents,
      openFile: store.editor.canvas.openFile?.filename,
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  },
  (store: EditorStore) => store.editor.canvas.interactionState,
  (store: EditorStore) => store.editor.jsxMetadata,
  (
    canvasState: InteractionCanvasState,
    interactionState: InteractionState | null,
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
  interactionState: InteractionState,
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
  (store: EditorStore): InteractionCanvasState => {
    return {
      selectedElements: store.editor.selectedViews,
      // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
      projectContents: store.editor.projectContents,
      openFile: store.editor.canvas.openFile?.filename,
      scale: store.editor.canvas.scale,
      canvasOffset: store.editor.canvas.roundedCanvasOffset,
    }
  },
  (store: EditorStore) => store.editor.canvas.interactionState,
  (store: EditorStore) => store.sessionStateState,
  (
    canvasState: InteractionCanvasState,
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
  interactionState: InteractionState,
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

function isStrategyApplicable(
  strategyName: string,
  canvasState: InteractionCanvasState,
  interactionState: InteractionState,
  sessionState: SessionStateState,
): boolean {
  const strategy = RegisteredCanvasStrategies.find((s) => s.name === strategyName)
  if (strategy == null) {
    return false
  }
  return strategy.fitness(canvasState, interactionState, sessionState) > 0
}

export function findCanvasStrategy(
  canvasState: InteractionCanvasState,
  interactionState: InteractionState,
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
      if (interactionData.drag == null || interactionData.prevDrag == null) {
        return interactionData
      } else {
        return {
          ...interactionData,
          dragStart: offsetPoint(interactionData.dragStart, interactionData.prevDrag),
          drag: pointDifference(interactionData.prevDrag, interactionData.drag),
          prevDrag: null,
        }
      }
    case 'KEYBOARD':
      return interactionData
    default:
      const _exhaustiveCheck: never = interactionData
      throw new Error(`Unhandled interaction type ${JSON.stringify(interactionData)}`)
  }
}

// Hard reset means we need to ignore everything happening in the interaction until now, and replay all the dragging
export function interactionDataHardReset(interactionData: InteractionData): InteractionData {
  switch (interactionData.type) {
    case 'DRAG':
      if (interactionData.drag == null) {
        return interactionData
      } else {
        const currentDrag = interactionData.drag ?? zeroCanvasPoint
        return {
          ...interactionData,
          dragStart: interactionData.originalDragStart,
          drag: pointDifference(
            interactionData.originalDragStart,
            offsetPoint(interactionData.dragStart, currentDrag),
          ),
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

// Hard reset means we need to ignore everything happening in the interaction until now, and replay all the dragging
export function interactionStateHardReset(interactionState: InteractionState): InteractionState {
  return {
    ...interactionState,
    interactionData: interactionDataHardReset(interactionState.interactionData),
  }
}

export function hasModifiersChanged(
  prevInteractionData: InteractionData | null,
  interactionData: InteractionData | null,
): boolean {
  return (
    interactionData?.type === 'DRAG' &&
    prevInteractionData?.type === 'DRAG' &&
    (interactionData.modifiers.alt !== prevInteractionData.modifiers.alt ||
      interactionData.modifiers.cmd !== prevInteractionData.modifiers.cmd ||
      interactionData.modifiers.ctrl !== prevInteractionData.modifiers.ctrl ||
      interactionData.modifiers.shift !== prevInteractionData.modifiers.shift)
  )
}

export function findCanvasStrategyFromDispatchResult(result: InnerDispatchResult) {
  const newEditorState = result.unpatchedEditor
  const canvasState: InteractionCanvasState = {
    selectedElements: newEditorState.selectedViews,
    // metadata: store.editor.jsxMetadata, // We can add metadata back if live metadata is necessary
    projectContents: newEditorState.projectContents,
    openFile: newEditorState.canvas.openFile?.filename,
    scale: newEditorState.canvas.scale,
    canvasOffset: newEditorState.canvas.roundedCanvasOffset,
  }
  const interactionState = newEditorState.canvas.interactionState
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
