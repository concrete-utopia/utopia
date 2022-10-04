import React from 'react'
import { createSelector } from 'reselect'
import { addAllUniquelyBy, mapDropNulls, sortBy } from '../../../core/shared/array-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { arrayEquals } from '../../../core/shared/utils'
import { AllElementProps, EditorState, EditorStorePatched } from '../../editor/store/editor-state'
import { useEditorState, useSelectorWithCallback } from '../../editor/store/store-hook'
import { absoluteMoveStrategy } from './absolute-move-strategy'
import {
  absoluteReparentStrategy,
  forcedAbsoluteReparentStrategy,
} from './absolute-reparent-strategy'
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
  InteractionLifecycle,
} from './canvas-strategy-types'
import { InteractionSession, StrategyState } from './interaction-state'
import { keyboardAbsoluteMoveStrategy } from './keyboard-absolute-move-strategy'
import { absoluteResizeBoundingBoxStrategy } from './absolute-resize-bounding-box-strategy'
import { keyboardAbsoluteResizeStrategy } from './keyboard-absolute-resize-strategy'
import { convertToAbsoluteAndMoveStrategy } from './convert-to-absolute-and-move-strategy'
import { flexReorderStrategy } from './flex-reorder-strategy'
import { absoluteDuplicateStrategy } from './absolute-duplicate-strategy'
import { absoluteReparentToFlexStrategy } from './absolute-reparent-to-flex-strategy'
import {
  flexReparentToAbsoluteStrategy,
  forcedFlexReparentToAbsoluteStrategy,
} from './flex-reparent-to-absolute-strategy'
import { flexReparentToFlexStrategy } from './flex-reparent-to-flex-strategy'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { flowReorderStrategy } from './flow-reorder-strategy'
import { isInsertMode } from '../../editor/editor-modes'
import { dragToInsertStrategy } from './drag-to-insert-strategy'
import { StateSelector } from 'zustand'
import { flowReorderWithIndicatorStrategy } from './flow-reorder-with-indicator-strategy'
import { flowReorderSliderStategy } from './flow-reorder-slider-strategy'
import { NonResizableControl } from '../controls/select-mode/non-resizable-control'
import { drawToInsertStrategy } from './draw-to-insert-strategy'
import { flexResizeBasicStrategy } from './flex-resize-basic-strategy'
import { optionalMap } from '../../../core/shared/optional-utils'
import { lookForApplicableParentStrategy } from './look-for-applicable-parent-strategy'

export type MetaCanvasStrategy = (
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
) => Array<CanvasStrategy>

export const existingStrategies: MetaCanvasStrategy = () => [
  absoluteMoveStrategy,
  absoluteReparentStrategy,
  forcedAbsoluteReparentStrategy,
  absoluteDuplicateStrategy,
  keyboardAbsoluteMoveStrategy,
  keyboardAbsoluteResizeStrategy,
  absoluteResizeBoundingBoxStrategy,
  flexReorderStrategy,
  flexReparentToAbsoluteStrategy,
  forcedFlexReparentToAbsoluteStrategy,
  flexReparentToFlexStrategy,
  convertToAbsoluteAndMoveStrategy,
  absoluteReparentToFlexStrategy,
  dragToInsertStrategy,
  flowReorderWithIndicatorStrategy,
  drawToInsertStrategy,
  flowReorderStrategy,
  flowReorderSliderStategy,
  flexResizeBasicStrategy,
]

export const RegisteredCanvasStrategies: Array<MetaCanvasStrategy> = [
  existingStrategies,
  lookForApplicableParentStrategy,
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

export interface ApplicableStrategy {
  strategy: CanvasStrategy
  name: string
}

export function applicableStrategy(strategy: CanvasStrategy, name: string): ApplicableStrategy {
  return {
    strategy: strategy,
    name: name,
  }
}

export function getApplicableStrategies(
  strategies: Array<MetaCanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession | null,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
): Array<CanvasStrategy> {
  return strategies
    .flatMap((s) => s(canvasState, interactionSession, metadata, allElementProps))
    .filter((strategy) => {
      return strategy.isApplicable(canvasState, interactionSession, metadata, allElementProps)
    })
}

const getApplicableStrategiesSelector = createSelector(
  (store: EditorStorePatched) =>
    optionalMap(
      (sas) => sas.map((s) => s.strategy),
      store.strategyState.sortedApplicableStrategies,
    ),
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

export function getApplicableStrategiesOrderedByFitness(
  strategies: Array<MetaCanvasStrategy>,
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

export interface FindCanvasStrategyResult {
  strategy: StrategyWithFitness | null
  previousStrategy: StrategyWithFitness | null
  sortedApplicableStrategies: Array<ApplicableStrategy>
}

export function findCanvasStrategy(
  strategies: Array<MetaCanvasStrategy>,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
  previousStrategyId: CanvasStrategyId | null,
): FindCanvasStrategyResult {
  const sortedApplicableStrategies = getApplicableStrategiesOrderedByFitness(
    strategies,
    canvasState,
    interactionSession,
    strategyState,
  )

  return {
    ...pickStrategy(sortedApplicableStrategies, interactionSession, previousStrategyId),
    sortedApplicableStrategies: sortedApplicableStrategies.map((s) => ({
      strategy: s.strategy,
      name: s.strategy.name(canvasState, interactionSession, strategyState),
    })),
  }
}

export function applyCanvasStrategy(
  strategy: CanvasStrategy,
  canvasState: InteractionCanvasState,
  interactionSession: InteractionSession,
  strategyState: StrategyState,
  strategyLifecycle: InteractionLifecycle,
): StrategyApplicationResult {
  return strategy.apply(canvasState, interactionSession, strategyState, strategyLifecycle)
}

export function useDelayedEditorState<T>(
  selector: StateSelector<EditorStorePatched, T | null>,
): T | null {
  /**
   * onMouseDown selection shows canvas controls that are active when a strategy runs with a delay (double click selection in hierarchy)
   * but when a drag threshold passes before the timer ends it shows up without delay
   */

  const [delayedValue, setDelayedValue] = React.useState<T | null>(null)
  const [timer, setTimer] = React.useState<number | null>(null)

  const immediateCallback = React.useCallback(
    (currentValue: T | null) => {
      setDelayedValue(currentValue)
      if (timer != null) {
        window.clearTimeout(timer)
        setTimer(null)
      }
    },
    [timer, setTimer, setDelayedValue],
  )

  const maybeDelayedCallback = React.useCallback(
    (currentValue: T | null) => {
      if (currentValue != null && delayedValue == null) {
        if (timer == null) {
          setTimer(
            window.setTimeout(() => {
              setDelayedValue(currentValue)
              setTimer(null)
            }, ControlDelay),
          )
        }
      } else {
        immediateCallback(currentValue)
      }
    },
    [immediateCallback, delayedValue, timer, setTimer, setDelayedValue],
  )

  useSelectorWithCallback(selector, maybeDelayedCallback)
  useSelectorWithCallback((store) => {
    if (
      store.editor.canvas.interactionSession?.interactionData.type === 'DRAG' &&
      store.editor.canvas.interactionSession?.interactionData.hasMouseMoved
    ) {
      return selector(store)
    } else {
      return null
    }
  }, immediateCallback)

  return delayedValue
}

export const useDelayedCurrentStrategy = () => {
  const selector = (store: EditorStorePatched) => store.strategyState.currentStrategy
  return useDelayedEditorState<CanvasStrategyId | null>(selector)
}

const notResizableControls: ControlWithKey = {
  control: NonResizableControl,
  key: 'not-resizable-control',
  show: 'visible-except-when-other-strategy-is-active',
}

export function getApplicableControls(
  currentStrategy: CanvasStrategyId | null,
  strategy: CanvasStrategy,
): Array<ControlWithKey> {
  return strategy.controlsToRender.filter((control) => {
    return (
      control.show === 'always-visible' ||
      (control.show === 'visible-only-while-active' && strategy.id === currentStrategy) ||
      (control.show === 'visible-except-when-other-strategy-is-active' &&
        (currentStrategy == null || strategy.id === currentStrategy))
    )
  })
}

export function isResizableStrategy(canvasStrategy: CanvasStrategy): boolean {
  switch (canvasStrategy.id) {
    case 'ABSOLUTE_RESIZE_BOUNDING_BOX':
    case 'KEYBOARD_ABSOLUTE_RESIZE':
    case 'FLEX_RESIZE_BASIC':
      return true
    default:
      return false
  }
}

export function interactionInProgress(interactionSession: InteractionSession | null): boolean {
  if (interactionSession == null) {
    return false
  } else {
    switch (interactionSession.interactionData.type) {
      case 'DRAG':
      case 'KEYBOARD':
      case 'HOVER':
        return true
      default:
        const _exhaustiveCheck: never = interactionSession.interactionData
        throw new Error(`Unhandled interaction data type: ${interactionSession.interactionData}`)
    }
  }
}

export function useGetApplicableStrategyControls(): Array<ControlWithKey> {
  const applicableStrategies = useGetApplicableStrategies()
  const currentStrategy = useDelayedCurrentStrategy()
  const currentlyInProgress = useEditorState((store) => {
    return interactionInProgress(store.editor.canvas.interactionSession)
  }, 'useGetApplicableStrategyControls currentlyInProgress')
  return React.useMemo(() => {
    let applicableControls: Array<ControlWithKey> = []
    let isResizable: boolean = false
    // Add the controls for currently applicable strategies.
    for (const strategy of applicableStrategies) {
      if (isResizableStrategy(strategy)) {
        isResizable = true
      }
      const strategyControls = getApplicableControls(currentStrategy, strategy)
      applicableControls = addAllUniquelyBy(
        applicableControls,
        strategyControls,
        (l, r) => l.control === r.control,
      )
    }
    // Special case controls.
    if (!isResizable && !currentlyInProgress) {
      applicableControls.push(notResizableControls)
    }
    return applicableControls
  }, [applicableStrategies, currentStrategy, currentlyInProgress])
}

export function isStrategyActive(strategyState: StrategyState): boolean {
  return (
    strategyState.accumulatedPatches.length > 0 || strategyState.currentStrategyCommands.length > 0
  )
}
