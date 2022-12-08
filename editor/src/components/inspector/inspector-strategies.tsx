import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import * as PP from '../../core/shared/property-path'
import { CanvasCommand } from '../canvas/commands/commands'
import { setProperty } from '../canvas/commands/set-property-command'
import { EditorDispatch } from '../editor/action-types'
import { FlexAlignment, FlexJustifyContent } from './inspector-common'
import { applyCommandsAction } from '../editor/actions/action-creators'

export type InspectorStrategy = (
  metadata: ElementInstanceMetadataMap,
  selectedElementPaths: Array<ElementPath>,
) => Array<CanvasCommand> | null

export const setFlexAlignJustifyContentStrategies = (
  flexAlignment: FlexAlignment,
  justifyContent: FlexJustifyContent,
): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = elementPaths.filter((e) =>
      MetadataUtils.isFlexLayoutedContainer(MetadataUtils.findElementByElementPath(metadata, e)),
    )

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => [
      setProperty('always', path, PP.create(['style', 'alignItems']), flexAlignment),
      setProperty('always', path, PP.create(['style', 'justifyContent']), justifyContent),
    ])
  },
]

export function runStrategies(
  dispatch: EditorDispatch,
  metadata: ElementInstanceMetadataMap,
  selectedViews: ElementPath[],
  strategies: InspectorStrategy[],
): void {
  for (const strategy of strategies) {
    const commands = strategy(metadata, selectedViews)
    if (commands != null) {
      dispatch([applyCommandsAction(commands)])
    }
    return
  }
}
