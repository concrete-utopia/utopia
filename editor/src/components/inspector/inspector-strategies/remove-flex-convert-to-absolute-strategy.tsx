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
  pruneFlexPropsCommands,
  sizeToVisualDimensions,
  styleP,
} from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

function positionAbsoluteRelativeToParentCommands(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return []
  }

  const left = element.specialSizeMeasurements.offset.x
  const top = element.specialSizeMeasurements.offset.y

  const parentFlexDirection = element.specialSizeMeasurements.parentFlexDirection

  return [
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('left'),
      setExplicitCssValue(cssPixelLength(left)),
      parentFlexDirection,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      styleP('top'),
      setExplicitCssValue(cssPixelLength(top)),
      parentFlexDirection,
    ),
    setProperty('always', elementPath, styleP('position'), 'absolute'),
  ]
}

function removeFlexConvertToAbsoluteOne(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const children = MetadataUtils.getChildrenPaths(metadata, elementPath)
  return [
    ...pruneFlexPropsCommands(flexContainerProps, elementPath), // flex-related stuff is pruned
    ...children.flatMap((c) => positionAbsoluteRelativeToParentCommands(metadata, c)), // all children are converted to absolute,
    ...children.flatMap((c) => sizeToVisualDimensions(metadata, c)), // with width/height based on measured dimensions
    ...children.flatMap((c) => pruneFlexPropsCommands(flexChildProps, c)),
    ...sizeToVisualDimensions(metadata, elementPath), // container is sized to keep its visual dimensions
  ]
}

export const removeFlexConvertToAbsolute: InspectorStrategy = {
  name: 'Remove flex layout and convert children to absolute',
  strategy: (metadata, elementPaths) => {
    const commands = filterKeepFlexContainers(metadata, elementPaths).flatMap((path) =>
      removeFlexConvertToAbsoluteOne(metadata, path),
    )
    return nullOrNonEmpty(commands)
  },
}
