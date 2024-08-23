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
  filterKeepGridContainers,
  gridContainerProps,
  gridElementProps,
} from '../inspector-common'
import type { InspectorStrategy } from './inspector-strategy'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import type { AllElementProps } from '../../editor/store/editor-state'

function removeFlexConvertToAbsoluteOne(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const children = MetadataUtils.getChildrenPathsOrdered(pathTrees, elementPath)
  return [
    ...prunePropsCommands(flexContainerProps, elementPath), // flex-related stuff is pruned
    ...children.flatMap((c) =>
      getConvertIndividualElementToAbsoluteCommandsFromMetadata(c, metadata, pathTrees),
    ), // all children are converted to absolute,
    ...sizeToVisualDimensions(metadata, pathTrees, elementPath), // container is sized to keep its visual dimensions
  ]
}

export const removeFlexConvertToAbsolute = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
): InspectorStrategy => ({
  name: 'Remove flex layout and convert children to absolute',
  strategy: () => {
    const commands = filterKeepFlexContainers(metadata, elementPaths).flatMap((path) =>
      removeFlexConvertToAbsoluteOne(metadata, pathTrees, allElementProps, path),
    )
    return nullOrNonEmpty(commands)
  },
})

export const removeGridConvertToAbsolute = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  pathTrees: ElementPathTrees,
): InspectorStrategy => ({
  name: 'Remove grid layout and convert children to absolute',
  strategy: () => {
    const commands = filterKeepGridContainers(metadata, elementPaths).flatMap((elementPath) => {
      const children = MetadataUtils.getChildrenPathsOrdered(pathTrees, elementPath)
      return [
        ...prunePropsCommands(gridContainerProps, elementPath),
        ...children.flatMap((c) =>
          getConvertIndividualElementToAbsoluteCommandsFromMetadata(c, metadata, pathTrees),
        ), // all children are converted to absolute,
        ...children.map((path) => deleteProperties('always', path, gridElementProps)), // remove grid-specific child props
        ...sizeToVisualDimensions(metadata, pathTrees, elementPath), // container is sized to keep its visual dimensions
      ]
    })
    return nullOrNonEmpty(commands)
  },
})
