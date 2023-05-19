import { ElementPathTreeRoot } from '../../../core/shared/element-path-tree'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { EditorDispatch } from '../../editor/action-types'
import { applyCommandsAction } from '../../editor/actions/action-creators'
import { AllElementProps } from '../../editor/store/editor-state'

export interface InspectorStrategy {
  name: string
  strategy: (
    metadata: ElementInstanceMetadataMap,
    selectedElementPaths: Array<ElementPath>,
    elementPathTree: ElementPathTreeRoot,
    allElementProps: AllElementProps,
  ) => Array<CanvasCommand> | null
}

export function commandsForFirstApplicableStrategy(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  elementPathTree: ElementPathTreeRoot,
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
  elementPathTree: ElementPathTreeRoot,
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
