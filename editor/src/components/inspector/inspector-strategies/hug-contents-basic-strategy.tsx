import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  Axis,
  detectFillHugFixedState,
  hugContentsApplicableForContainer,
  nukeSizingPropsForAxisCommand,
  sizeToVisualDimensions,
  widthHeightFromAxis,
} from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

function hugContentsSingleElement(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const basicCommands = [
    nukeSizingPropsForAxisCommand(axis, elementPath),
    setProperty(
      'always',
      elementPath,
      PP.create(['style', widthHeightFromAxis(axis)]),
      'min-content',
    ),
  ]

  const chilren = MetadataUtils.getChildrenPaths(metadata, elementPath)
  const transformChildrenToFixedCommands = chilren.flatMap((child) => {
    const state = detectFillHugFixedState(axis, metadata, child)
    if (state == null || state.type === 'fill') {
      return sizeToVisualDimensions(metadata, child)
    }
    return []
  })

  return [...basicCommands, ...transformChildrenToFixedCommands]
}

export const hugContentsBasicStrategy = (axis: Axis): InspectorStrategy => ({
  name: 'Set to Hug',
  strategy: (metadata, elementPaths) => {
    const elements = elementPaths.filter((path) =>
      hugContentsApplicableForContainer(metadata, path),
    )

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => hugContentsSingleElement(axis, metadata, path))
  },
})
