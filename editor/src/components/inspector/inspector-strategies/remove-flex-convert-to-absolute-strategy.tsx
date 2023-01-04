import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { InspectorStrategy } from './inspector-strategy'

function removeFlexConvertToAbsoluteOne(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  // all children are converted to absolute, with width/height based on measured dimensions
  return []
}

export const removeFlexConvertToAbsolute: InspectorStrategy = (metadata, elementPaths) => {
  return elementPaths.flatMap((path) => removeFlexConvertToAbsoluteOne(metadata, path))
}
