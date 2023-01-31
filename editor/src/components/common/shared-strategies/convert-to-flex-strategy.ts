import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  convertWidthToFlexGrowOptionally,
  nukeAllAbsolutePositioningPropsCommands,
  sizeToVisualDimensions,
} from '../../inspector/inspector-common'
import { setHugContentForAxis } from '../../inspector/inspector-strategies/hug-contents-basic-strategy'

export function convertLayoutToFlexCommands(
  metadata: ElementInstanceMetadataMap,
  elementPaths: Array<ElementPath>,
): Array<CanvasCommand> {
  return elementPaths.flatMap((path) => [
    setProperty('always', path, PP.create('style', 'display'), 'flex'),
    setProperty('always', path, PP.create('style', 'gap'), 15),
    setHugContentForAxis('horizontal', path),
    setHugContentForAxis('vertical', path),
    ...MetadataUtils.getChildrenPaths(metadata, path).flatMap((child) => [
      ...nukeAllAbsolutePositioningPropsCommands(child),
      ...sizeToVisualDimensions(metadata, child),
      ...convertWidthToFlexGrowOptionally(metadata, child),
    ]),
  ])
}
