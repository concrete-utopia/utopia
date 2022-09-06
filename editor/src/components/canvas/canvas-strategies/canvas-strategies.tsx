import React from 'react'
import { createSelector } from 'reselect'
import { addAllUniquelyBy, mapDropNulls, sortBy } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { arrayEquals } from '../../../core/shared/utils'
import { InnerDispatchResult } from '../../editor/store/dispatch'
import { AllElementProps, EditorState, EditorStorePatched } from '../../editor/store/editor-state'
import { useEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'
import { CanvasCommand } from '../commands/commands'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import { absoluteReparentStrategy } from './absolute-reparent-strategy'
import {
  CanvasStrategy,
  CanvasStrategyId,
  ControlDelay,
  ControlWithKey,
  insertionSubjects,
  InteractionCanvasState,
  InteractionTarget,
  targetPaths,
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
import {
  flowReorderAutoConversionStategy,
  flowReorderNoConversionStategy,
  flowReorderSameTypeOnlyStategy,
} from './flow-reorder-strategy'
import { isInsertMode } from '../../editor/editor-modes'
import { dragToInsertStrategy } from './drag-to-insert-strategy'

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
  dragToInsertStrategy,
  flowReorderAutoConversionStategy,
  flowReorderNoConversionStategy,
  flowReorderSameTypeOnlyStategy,
]

export function pickCanvasStateFromEditorState(
  editorState: EditorState,
  builtInDependencies: BuiltInDependencies,
): InteractionCanvasState {
  return {
    builtInDependencies: builtInDependencies,
    interactionTarget: getInteractionTargetFromEditorState(editorState),
    projectContents: editorState.projectContents,
    nodeModules: editorState.nodeModules.files,
    openFile: editorState.canvas.openFile?.filename,
    scale: editorState.canvas.scale,
    canvasOffset: editorState.canvas.roundedCanvasOffset,
  }
}

function getInteractionTargetFromEditorState(editor: EditorState): InteractionTarget {
  if (isInsertMode(editor.mode)) {
    return insertionSubjects([editor.mode.subject])
  } else {
    return targetPaths(editor.selectedViews)
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
  (store: EditorStorePatched) => store.strategyState.sortedApplicableStrategies,
  (store: EditorStorePatched): InteractionCanvasState => {
    return pickCanvasStateFromEditorState(store.editor, store.builtInDependencies)
  },
  (store: EditorStorePatched) => store.editor.canvas.interactionSession,
  (store: EditorStorePatched) => store.editor.jsxMetadata,
  (store: EditorStorePatched) => store.editor.allElementProps,
  (
    applicableStrategiesFromStrategyState: Array<CanvasStrategy> | null,
    canvasState: InteractionCanvasState,
    interactionSession: InteractionSession | null,
    metadata: ElementInstanceMetadataMap,
    allElementProps: AllElementProps,
  ): Array<CanvasStrategy> => {
    if (applicableStrategiesFromStrategyState != null) {
      return applicableStrategiesFromStrategyState
    } else {
      return getApplicableStrategies(
        RegisteredCanvasStrategies,
        canvasState,
        interactionSession,
        metadata,
        allElementProps,
      )
    }
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

export const useDelayedCurrentStrategy = () => {
  /**
   * onMouseDown selection shows canvas controls that are active when a strategy runs with a delay (double click selection in hierarchy)
   * but when a drag threshold passes before the timer ends it shows up without delay
   */
  const [delayedStrategyValue, setDelayedStrategyValue] = React.useState<CanvasStrategyId | null>(
    null,
  )
  const [timer, setTimer] = React.useState<number | null>(null)

  const immediateCallback = React.useCallback(
    (currentStrategy: CanvasStrategyId | null) => {
      setDelayedStrategyValue(currentStrategy)
      if (timer != null) {
        window.clearTimeout(timer)
        setTimer(null)
      }
    },
    [timer, setTimer, setDelayedStrategyValue],
  )

  const maybeDelayedCallback = React.useCallback(
    (currentStrategy: CanvasStrategyId | null) => {
      if (currentStrategy != null && delayedStrategyValue == null) {
        if (timer == null) {
          setTimer(
            window.setTimeout(() => {
              setDelayedStrategyValue(currentStrategy)
              setTimer(null)
            }, ControlDelay),
          )
        }
      } else {
        immediateCallback(currentStrategy)
      }
    },
    [immediateCallback, delayedStrategyValue, timer, setTimer, setDelayedStrategyValue],
  )

  useSelectorWithCallback((store) => store.strategyState.currentStrategy, maybeDelayedCallback)
  useSelectorWithCallback((store) => {
    if (
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG' &&
      store.editor.canvas.interactionSession?.interactionData.hasMouseMoved
    ) {
      return store.strategyState.currentStrategy
    } else {
      return null
    }
  }, immediateCallback)

  return delayedStrategyValue
}

export function useGetApplicableStrategyControls(): Array<ControlWithKey> {
  const applicableStrategies = useGetApplicableStrategies()
  const currentStrategy = useDelayedCurrentStrategy()
  return React.useMemo(() => {
    return applicableStrategies.reduce<ControlWithKey[]>((working, s) => {
      const filteredControls = s.controlsToRender.filter(
        (control) =>
          control.show === 'always-visible' ||
          (control.show === 'visible-only-while-active' && s.id === currentStrategy) ||
          (control.show === 'visible-except-when-other-strategy-is-active' &&
            (currentStrategy == null || s.id === currentStrategy)),
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
