import * as PP from '../../../core/shared/property-path'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  widthHeightFromAxis,
  Axis,
  nullOrNonEmpty,
  hugContentsApplicableForText,
  nukeSizingPropsForAxisCommand,
  MaxContent,
} from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

const hugContentsTextStrategyI = (
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> => {
  if (!hugContentsApplicableForText(metadata, elementPath)) {
    return []
  }
  return [
    nukeSizingPropsForAxisCommand(axis, elementPath),
    setProperty('always', elementPath, PP.create(['style', widthHeightFromAxis(axis)]), MaxContent),
  ]
}

export const hugContentsTextStrategy = (axis: Axis): InspectorStrategy => ({
  name: 'Hug text contents',
  strategy: (metadata, elementPaths) => {
    const commands = elementPaths.flatMap((path) => hugContentsTextStrategyI(axis, metadata, path))
    return nullOrNonEmpty(commands)
  },
})
