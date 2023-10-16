import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { CanvasCommand } from '../../canvas/commands/commands'
import type { EditorDispatch } from '../../editor/action-types'
import { applyCommandsAction } from '../../editor/actions/action-creators'
import type { AllElementProps } from '../../editor/store/editor-state'

export interface InspectorStrategyResult<T> {
  commands: Array<CanvasCommand>
  data: T
}

export interface InspectorStrategy<T> {
  name: string
  strategy: (
    metadata: ElementInstanceMetadataMap,
    selectedElementPaths: Array<ElementPath>,
    elementPathTree: ElementPathTrees,
    allElementProps: AllElementProps,
  ) => InspectorStrategyResult<T> | null
}

export function resultForFirstApplicableStrategy<T>(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  strategies: Array<InspectorStrategy<T>>,
): InspectorStrategyResult<T> | null {
  for (const strategy of strategies) {
    const result = strategy.strategy(metadata, selectedViews, elementPathTree, allElementProps)
    if (result != null) {
      return result
    }
  }
  return null
}

export function commandsForFirstApplicableStrategy<T>(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  strategies: Array<InspectorStrategy<T>>,
): Array<CanvasCommand> | null {
  const result = resultForFirstApplicableStrategy(
    metadata,
    selectedViews,
    elementPathTree,
    allElementProps,
    strategies,
  )
  return result?.commands ?? null
}

export function executeFirstApplicableStrategy<T>(
  dispatch: EditorDispatch,
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  strategies: InspectorStrategy<T>[],
): void {
  const result = resultForFirstApplicableStrategy(
    metadata,
    selectedViews,
    elementPathTree,
    allElementProps,
    strategies,
  )
  if (result != null) {
    dispatch([applyCommandsAction(result.commands)])
  }
}
