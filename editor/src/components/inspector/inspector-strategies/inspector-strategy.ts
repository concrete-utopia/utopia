import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { CanvasCommand } from '../../canvas/commands/commands'
import type { EditorDispatch } from '../../editor/action-types'
import { applyCommandsAction } from '../../editor/actions/action-creators'
import type { AllElementProps } from '../../editor/store/editor-state'

interface CustomInspectorStrategyResultBase {
  commands: Array<CanvasCommand>
}

export type CustomInspectorStrategyResult<T extends undefined | Record<string, unknown>> =
  T extends undefined
    ? CustomInspectorStrategyResultBase
    : CustomInspectorStrategyResultBase & { data: T }

export interface CustomInspectorStrategy<T extends undefined | Record<string, unknown>> {
  name: string
  strategy: (
    metadata: ElementInstanceMetadataMap,
    selectedElementPaths: Array<ElementPath>,
    elementPathTree: ElementPathTrees,
    allElementProps: AllElementProps,
  ) => CustomInspectorStrategyResult<T> | null
}

export interface InspectorStrategy {
  name: string
  strategy: (
    metadata: ElementInstanceMetadataMap,
    selectedElementPaths: Array<ElementPath>,
    elementPathTree: ElementPathTrees,
    allElementProps: AllElementProps,
  ) => Array<CanvasCommand> | null
}

export interface MultiPhaseInspectorStrategy {
  name: string
  strategy: (
    metadata: ElementInstanceMetadataMap,
    selectedElementPaths: Array<ElementPath>,
    elementPathTree: ElementPathTrees,
    allElementProps: AllElementProps,
  ) => Generator<Array<CanvasCommand> | null, void>
}

export function resultForFirstApplicableStrategy<T extends undefined | Record<string, unknown>>(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  strategies: Array<CustomInspectorStrategy<T>>,
): CustomInspectorStrategyResult<T> | null {
  for (const strategy of strategies) {
    const result = strategy.strategy(metadata, selectedViews, elementPathTree, allElementProps)
    if (result != null) {
      return result
    }
  }
  return null
}

export function commandsForFirstApplicableStrategy(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  strategies: Array<InspectorStrategy>,
): Array<CanvasCommand> | null {
  for (const strategy of strategies) {
    const commands = strategy.strategy(metadata, selectedViews, elementPathTree, allElementProps)
    if (commands != null) {
      return commands
    }
  }
  return null
}

export function executeFirstApplicableStrategy(
  dispatch: EditorDispatch,
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  strategies: InspectorStrategy[],
): void {
  const commands = commandsForFirstApplicableStrategy(
    metadata,
    selectedViews,
    elementPathTree,
    allElementProps,
    strategies,
  )
  if (commands != null) {
    dispatch([applyCommandsAction(commands)])
  }
}

export function executeFirstMultiPhaseStrategy(
  dispatch: EditorDispatch,
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  strategies: MultiPhaseInspectorStrategy[],
): void {
  function executeMultiPhaseStrategy(strategy: MultiPhaseInspectorStrategy): 'success' | 'fail' {
    for (const commands of strategy.strategy(
      metadata,
      selectedViews,
      elementPathTree,
      allElementProps,
    )) {
      if (commands == null) {
        return 'fail'
      }
      dispatch([applyCommandsAction(commands)])
    }
    return 'success'
  }

  for (const strategy of strategies) {
    const result = executeMultiPhaseStrategy(strategy)
    if (result === 'success') {
      return
    }
  }
}
