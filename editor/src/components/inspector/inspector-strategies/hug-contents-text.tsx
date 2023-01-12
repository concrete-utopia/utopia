import * as PP from '../../../core/shared/property-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { elementOnlyHasTextChildren } from '../../../core/model/element-template-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { optionalMap } from '../../../core/shared/optional-utils'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import { widthHeightFromAxis, Axis, nullOrNonEmpty } from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

const hugContentsTextStrategyI = (
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> => {
  const element = MetadataUtils.getJSXElementFromMetadata(metadata, elementPath)
  const applicable = optionalMap(elementOnlyHasTextChildren, element) === true
  if (!applicable) {
    return []
  }
  return [
    setProperty(
      'always',
      elementPath,
      PP.create(['style', widthHeightFromAxis(axis)]),
      'max-content',
    ),
  ]
}

export const hugContentsTextStrategy =
  (axis: Axis): InspectorStrategy =>
  (metadata, elementPaths) => {
    const commands = elementPaths.flatMap((path) => hugContentsTextStrategyI(axis, metadata, path))
    return nullOrNonEmpty(commands)
  }
