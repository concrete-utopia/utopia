import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { EditorDispatch } from '../../editor/action-types'
import { applyCommandsAction } from '../../editor/actions/action-creators'

export interface InspectorStrategy {
  name: string
  strategy: (
    metadata: ElementInstanceMetadataMap,
    selectedElementPaths: Array<ElementPath>,
  ) => Array<CanvasCommand> | null
}

export function executeFirstApplicableStrategy(
  dispatch: EditorDispatch,
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  strategies: InspectorStrategy[],
): void {
  for (const strategy of strategies) {
    const commands = strategy.strategy(metadata, selectedViews)
    if (commands != null) {
      return dispatch([applyCommandsAction(commands)])
    }
  }
}
