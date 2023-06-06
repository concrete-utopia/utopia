import { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import { setProperty } from '../../canvas/commands/set-property-command'
import { cssPixelLength } from '../common/css-utils'
import {
  filterKeepFlexContainers,
  flexChildProps,
  flexContainerProps,
  nullOrNonEmpty,
  addPositionAbsoluteTopLeft,
  pruneFlexPropsCommands,
  sizeToVisualDimensions,
} from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

function removeFlexConvertToAbsoluteOne(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const children = MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath)
  return [
    ...pruneFlexPropsCommands(flexContainerProps, elementPath), // flex-related stuff is pruned
    ...children.flatMap((c) => addPositionAbsoluteTopLeft(metadata, c)), // all children are converted to absolute,
    ...children.flatMap((c) => sizeToVisualDimensions(metadata, c)), // with width/height based on measured dimensions
    ...children.flatMap((c) => pruneFlexPropsCommands(flexChildProps, c)),
    ...sizeToVisualDimensions(metadata, elementPath), // container is sized to keep its visual dimensions
  ]
}

export const removeFlexConvertToAbsolute: InspectorStrategy = {
  name: 'Remove flex layout and convert children to absolute',
  strategy: (metadata, elementPaths, pathTrees) => {
    const commands = filterKeepFlexContainers(metadata, elementPaths).flatMap((path) =>
      removeFlexConvertToAbsoluteOne(metadata, pathTrees, path),
    )
    return nullOrNonEmpty(commands)
  },
}
