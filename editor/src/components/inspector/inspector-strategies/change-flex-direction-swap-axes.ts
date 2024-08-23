import * as PP from '../../../core/shared/property-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { CanvasCommand } from '../../canvas/commands/commands'
import { setProperty } from '../../canvas/commands/set-property-command'
import type { FlexDirection } from '../common/css-utils'
import {
  detectFillHugFixedState,
  detectFlexDirectionOne,
  nullOrNonEmpty,
} from '../inspector-common'
import { fillContainerStrategyFlexParent } from './fill-container-basic-strategy'
import { fixedSizeBasicStrategy } from './fixed-size-basic-strategy'
import type { InspectorStrategy } from './inspector-strategy'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { AllElementProps } from '../../../components/editor/store/editor-state'

function swapAxesCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  selectedElement: ElementPath,
  currentFlexDirection: FlexDirection | null,
  flexDirectionToBeApplied: FlexDirection,
): Array<CanvasCommand> {
  const horizontalSizing = detectFillHugFixedState(
    'horizontal',
    metadata,
    selectedElement,
  ).fixedHugFill
  const verticalSizing = detectFillHugFixedState('vertical', metadata, selectedElement).fixedHugFill
  if (
    flexDirectionToBeApplied.startsWith('col') &&
    currentFlexDirection?.startsWith('row') &&
    horizontalSizing?.type === 'fill' &&
    verticalSizing?.type === 'fixed'
  ) {
    return [
      ...(fixedSizeBasicStrategy(
        'always',
        metadata,
        [selectedElement],
        'horizontal',
        verticalSizing.value,
      ).strategy() ?? []),
      ...(fillContainerStrategyFlexParent(
        metadata,
        allElementProps,
        pathTrees,
        [selectedElement],
        'vertical',
        'default',
        {
          forceFlexDirectionForParent: flexDirectionToBeApplied,
        },
      ).strategy() ?? []),
    ]
  }

  if (
    flexDirectionToBeApplied.startsWith('row') &&
    currentFlexDirection?.startsWith('col') &&
    verticalSizing?.type === 'fill' &&
    horizontalSizing?.type === 'fixed'
  ) {
    return [
      ...(fixedSizeBasicStrategy(
        'always',
        metadata,
        [selectedElement],
        'vertical',
        horizontalSizing.value,
      ).strategy() ?? []),
      ...(fillContainerStrategyFlexParent(
        metadata,
        allElementProps,
        pathTrees,
        [selectedElement],
        'horizontal',
        'default',
        {
          forceFlexDirectionForParent: flexDirectionToBeApplied,
        },
      ).strategy() ?? []),
    ]
  }

  return []
}

function setFlexDirectionSwapAxesSingleElement(
  direction: FlexDirection,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
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

  const commands = MetadataUtils.getChildrenPathsOrdered(elementPathTree, selectedElement).flatMap(
    (child) =>
      swapAxesCommands(
        metadata,
        allElementProps,
        elementPathTree,
        child,
        currentFlexDirection,
        direction,
      ),
  )

  if (commands.length === 0) {
    return []
  }

  return [
    ...commands,
    setProperty('always', selectedElement, PP.create('style', 'flexDirection'), direction),
  ]
}

export const setFlexDirectionSwapAxes = (
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTree: ElementPathTrees,
  selectedElementPaths: ElementPath[],
  direction: FlexDirection,
): InspectorStrategy => ({
  name: 'Swap fill axes',
  strategy: () =>
    nullOrNonEmpty(
      selectedElementPaths.flatMap((path) =>
        setFlexDirectionSwapAxesSingleElement(
          direction,
          metadata,
          allElementProps,
          elementPathTree,
          path,
        ),
      ),
    ),
})
