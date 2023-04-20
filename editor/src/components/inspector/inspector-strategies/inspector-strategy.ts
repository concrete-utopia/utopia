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
    allElementProps: AllElementProps,
  ) => Array<CanvasCommand> | null
}

export function commandsForFirstApplicableStrategy(
  metadata: ElementInstanceMetadataMap,
  selectedViews: Array<ElementPath>,
  allElementProps: AllElementProps,
  strategies: Array<InspectorStrategy>,
): Array<CanvasCommand> | null {
  for (const strategy of strategies) {
    const commands = strategy.strategy(metadata, selectedViews, allElementProps)
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
  allElementProps: AllElementProps,
  strategies: InspectorStrategy[],
): void {
  const commands = commandsForFirstApplicableStrategy(
    metadata,
    selectedViews,
    allElementProps,
    strategies,
  )
  if (commands != null) {
    dispatch([applyCommandsAction(commands)])
  }
}
