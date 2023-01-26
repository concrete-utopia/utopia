import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import { showToastCommand } from '../../canvas/commands/show-toast-command'
import {
  Axis,
  detectFillHugFixedState,
  hugContentsApplicableForContainer,
  hugContentsApplicableForText,
  MaxContent,
  nukeSizingPropsForAxisCommand,
  sizeToVisualDimensions,
  widthHeightFromAxis,
} from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'

const CHILDREN_CONVERTED_TOAST_ID = 'CHILDREN_CONVERTED_TOAST_ID'

function hugContentsSingleElement(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const basicCommands = [
    nukeSizingPropsForAxisCommand(axis, elementPath),
    setProperty('always', elementPath, PP.create('style', widthHeightFromAxis(axis)), MaxContent),
  ]

  const chilren = MetadataUtils.getChildrenPaths(metadata, elementPath)
  const transformChildrenToFixedCommands = chilren.flatMap((child) => {
    const state = detectFillHugFixedState(axis, metadata, child)
    if (state?.type === 'fixed' || state?.type === 'hug') {
      return []
    }
    return [
      ...sizeToVisualDimensions(metadata, child),
      showToastCommand('Children converted to fixed size', 'INFO', CHILDREN_CONVERTED_TOAST_ID),
    ]
  })

  return [...basicCommands, ...transformChildrenToFixedCommands]
}

export const hugContentsBasicStrategy = (axis: Axis): InspectorStrategy => ({
  name: 'Set to Hug',
  strategy: (metadata, elementPaths) => {
    const elements = elementPaths.filter(
      (path) =>
        hugContentsApplicableForContainer(metadata, path) ||
        hugContentsApplicableForText(metadata, path),
    )

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => hugContentsSingleElement(axis, metadata, path))
  },
})
