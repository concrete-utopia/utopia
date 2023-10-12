import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { CanvasCommand } from '../../canvas/commands/commands'
import {
  filterKeepFlexContainers,
  flexContainerProps,
  nullOrNonEmpty,
  prunePropsCommands,
  sizeToVisualDimensions,
  getConvertIndividualElementToAbsoluteCommandsFromMetadata,
} from '../inspector-common'
import type { InspectorStrategy } from './inspector-strategy'

function removeFlexConvertToAbsoluteOne(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const children = MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath)
  return [
    ...prunePropsCommands(flexContainerProps, elementPath), // flex-related stuff is pruned
    ...children.flatMap((c) =>
      getConvertIndividualElementToAbsoluteCommandsFromMetadata(c, metadata, pathTrees),
    ), // all children are converted to absolute,
    ...sizeToVisualDimensions(metadata, pathTrees, elementPath), // container is sized to keep its visual dimensions
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
