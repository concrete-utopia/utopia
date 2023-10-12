import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import * as PP from '../../../core/shared/property-path'
import type { CanvasCommand } from '../../canvas/commands/commands'
import type { SetCssLengthProperty } from '../../canvas/commands/set-css-length-command'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import { SetProperty, setProperty } from '../../canvas/commands/set-property-command'
import { showToastCommand } from '../../canvas/commands/show-toast-command'
import type { FlexDirection } from '../common/css-utils'
import { cssKeyword, cssUnitlessLength } from '../common/css-utils'
import type { Axis } from '../inspector-common'
import {
  detectFillHugFixedState,
  hugContentsApplicableForContainer,
  hugContentsApplicableForText,
  MaxContent,
  nukeSizingPropsForAxisCommand,
  removeExtraPinsWhenSettingSize,
  sizeToVisualDimensions,
  widthHeightFromAxis,
} from '../inspector-common'
import type { InspectorStrategy } from './inspector-strategy'
import { queueTrueUpElement } from '../../canvas/commands/queue-true-up-command'
import { trueUpGroupElementChanged } from '../../../components/editor/store/editor-state'

const CHILDREN_CONVERTED_TOAST_ID = 'CHILDREN_CONVERTED_TOAST_ID'

export function setHugContentForAxis(
  axis: Axis,
  target: ElementPath,
  parentFlexDirection: FlexDirection | null,
): SetCssLengthProperty {
  return setCssLengthProperty(
    'always',
    target,
    PP.create('style', widthHeightFromAxis(axis)),
    setExplicitCssValue(cssKeyword(MaxContent)),
    parentFlexDirection,
  )
}

function hugContentsSingleElement(
  axis: Axis,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, elementPath)

  const basicCommands = [
    ...removeExtraPinsWhenSettingSize(axis, elementMetadata),
    nukeSizingPropsForAxisCommand(axis, elementPath),
    setHugContentForAxis(
      axis,
      elementPath,
      elementMetadata?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
  ]

  const children = MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath)
  const transformChildrenToFixedCommands = children.flatMap((child) => {
    const state = detectFillHugFixedState(axis, metadata, child).fixedHugFill
    if (state?.type === 'fixed' || state?.type === 'hug') {
      return []
    }
    return [
      ...sizeToVisualDimensions(metadata, pathTrees, child),
      showToastCommand('Children converted to fixed size', 'INFO', CHILDREN_CONVERTED_TOAST_ID),
    ]
  })

  return [
    ...basicCommands,
    ...transformChildrenToFixedCommands,
    queueTrueUpElement([trueUpGroupElementChanged(elementPath)]),
  ]
}

export const hugContentsBasicStrategy = (axis: Axis): InspectorStrategy => ({
  name: 'Set to Hug',
  strategy: (metadata, elementPaths, pathTrees) => {
    const elements = elementPaths.filter(
      (path) =>
        hugContentsApplicableForContainer(metadata, pathTrees, path) ||
        hugContentsApplicableForText(metadata, path),
    )

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => hugContentsSingleElement(axis, metadata, pathTrees, path))
  },
})
