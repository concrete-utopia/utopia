import * as PP from '../../../core/shared/property-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import { FlexDirection } from '../common/css-utils'
import {
  detectFillHugFixedState,
  detectFlexDirectionOne,
  nullOrNonEmpty,
} from '../inspector-common'
import { fillContainerStrategyFlexParent } from './fill-container-basic-strategy'
import { fixedSizeBasicStrategy } from './fixed-size-basic-strategy'
import { InspectorStrategy } from './inspector-strategy'

function setFlexDirectionSwapAxesSingleElement(
  direction: FlexDirection,
  metadata: ElementInstanceMetadataMap,
  selectedElement: ElementPath,
): Array<CanvasCommand> {
  if (
    !MetadataUtils.isFlexLayoutedContainer(
      MetadataUtils.findElementByElementPath(metadata, selectedElement),
    )
  ) {
    return []
  }

  const currentFlexDirection = detectFlexDirectionOne(metadata, selectedElement)

  const commands = MetadataUtils.getChildrenPaths(metadata, selectedElement).flatMap((child) => {
    const horizontalSizing = detectFillHugFixedState('horizontal', metadata, child)
    const verticalSizing = detectFillHugFixedState('vertical', metadata, child)
    if (
      direction.startsWith('col') &&
      currentFlexDirection?.startsWith('row') &&
      horizontalSizing?.type === 'fill' &&
      verticalSizing?.type === 'fixed'
    ) {
      return [
        ...(fixedSizeBasicStrategy('always', 'horizontal', verticalSizing.value).strategy(
          metadata,
          [child],
        ) ?? []),
        ...(fillContainerStrategyFlexParent('vertical', 'default', {
          forceFlexDirectionForParent: direction,
        }).strategy(metadata, [child]) ?? []),
      ]
    }

    if (
      direction.startsWith('row') &&
      currentFlexDirection?.startsWith('col') &&
      verticalSizing?.type === 'fill' &&
      horizontalSizing?.type === 'fixed'
    ) {
      return [
        ...(fixedSizeBasicStrategy('always', 'vertical', horizontalSizing.value).strategy(
          metadata,
          [child],
        ) ?? []),
        ...(fillContainerStrategyFlexParent('horizontal', 'default', {
          forceFlexDirectionForParent: direction,
        }).strategy(metadata, [child]) ?? []),
      ]
    }

    return []
  })

  if (commands.length === 0) {
    return []
  }

  return [
    ...commands,
    setProperty('always', selectedElement, PP.create('style', 'flexDirection'), direction),
  ]
}

export const setFlexDirectionSwapAxes = (direction: FlexDirection): InspectorStrategy => ({
  name: 'Swap fill axes',
  strategy: (metadata, selectedElementPaths) =>
    nullOrNonEmpty(
      selectedElementPaths.flatMap((path) =>
        setFlexDirectionSwapAxesSingleElement(direction, metadata, path),
      ),
    ),
})
