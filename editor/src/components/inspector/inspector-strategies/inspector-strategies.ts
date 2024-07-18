import * as PP from '../../../core/shared/property-path'
import { setProperty } from '../../canvas/commands/set-property-command'
import type { Axis, FlexAlignment, FlexJustifyContent } from '../inspector-common'
import {
  filterKeepFlexContainers,
  flexChildProps,
  prunePropsCommands,
  sizeToVisualDimensions,
} from '../inspector-common'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import type { CSSNumber, FlexDirection } from '../common/css-utils'
import { removeFlexConvertToAbsolute } from './remove-flex-convert-to-absolute-strategy'
import type { InspectorStrategy } from './inspector-strategy'
import type { WhenToRun } from '../../../components/canvas/commands/commands'
import {
  hugContentsAbsoluteStrategy,
  hugContentsBasicStrategy,
  hugContentsGridStrategy,
} from './hug-contents-strategy'
import {
  fillContainerStrategyFlexParent,
  fillContainerStrategyFlow,
} from './fill-container-basic-strategy'
import { setSpacingModePacked, setSpacingModeSpaceBetween } from './spacing-mode-strategies'
import { convertLayoutToFlexCommands } from '../../common/shared-strategies/convert-to-flex-strategy'
import { fixedSizeBasicStrategy } from './fixed-size-basic-strategy'
import { setFlexDirectionSwapAxes } from './change-flex-direction-swap-axes'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { AllElementProps } from '../../editor/store/editor-state'

export const setFlexAlignStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  flexAlignment: FlexAlignment,
): Array<InspectorStrategy> => [
  {
    name: 'Set flex-align',
    strategy: () => {
      const elements = filterKeepFlexContainers(metadata, elementPaths)

      if (elements.length === 0) {
        return null
      }

      return elements.flatMap((path) => [
        setProperty('always', path, PP.create('style', 'alignItems'), flexAlignment),
      ])
    },
  },
]

export const setJustifyContentStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  justifyContent: FlexJustifyContent,
): Array<InspectorStrategy> => [
  {
    name: 'Set justify-content',
    strategy: () => {
      const elements = filterKeepFlexContainers(metadata, elementPaths)

      if (elements.length === 0) {
        return null
      }

      return elements.flatMap((path) => [
        setProperty('always', path, PP.create('style', 'justifyContent'), justifyContent),
      ])
    },
  },
]

export const setFlexAlignJustifyContentStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  flexAlignment: FlexAlignment,
  justifyContent: FlexJustifyContent,
): Array<InspectorStrategy> => [
  {
    name: 'Set flex-align and justify-content',
    strategy: () => {
      const elements = filterKeepFlexContainers(metadata, elementPaths)

      if (elements.length === 0) {
        return null
      }

      return elements.flatMap((path) => [
        setProperty('always', path, PP.create('style', 'alignItems'), flexAlignment),
        setProperty('always', path, PP.create('style', 'justifyContent'), justifyContent),
      ])
    },
  },
]

export const removeFlexDirectionStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
): Array<InspectorStrategy> => [
  {
    name: 'Unset flex direction',
    strategy: () => {
      const elements = filterKeepFlexContainers(metadata, elementPaths)

      if (elements.length === 0) {
        return null
      }

      return elements.map((path) =>
        deleteProperties('always', path, [PP.create('style', 'flexDirection')]),
      )
    },
  },
]

export const updateFlexDirectionStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  pathTrees: ElementPathTrees,
  flexDirection: FlexDirection,
): Array<InspectorStrategy> => [
  setFlexDirectionSwapAxes(metadata, pathTrees, elementPaths, flexDirection),
  {
    name: 'Set flex direction',
    strategy: () => {
      const elements = filterKeepFlexContainers(metadata, elementPaths)

      if (elements.length === 0) {
        return null
      }

      return elements.flatMap((path) => [
        setProperty('always', path, PP.create('style', 'flexDirection'), flexDirection),
        ...MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, path).flatMap((child) => [
          ...prunePropsCommands(flexChildProps, child),
          ...sizeToVisualDimensions(metadata, pathTrees, child),
        ]),
      ])
    },
  },
]

export const addFlexLayoutStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
): Array<InspectorStrategy> => [
  {
    name: 'Add flex layout',
    strategy: () => {
      return convertLayoutToFlexCommands(metadata, elementPathTree, elementPaths, allElementProps)
    },
  },
]

export const removeFlexLayoutStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  pathTrees: ElementPathTrees,
): Array<InspectorStrategy> => [
  removeFlexConvertToAbsolute(metadata, elementPaths, pathTrees),
  {
    name: 'Remove flex layout',
    strategy: () => {
      const elements = filterKeepFlexContainers(metadata, elementPaths)

      if (elements.length === 0) {
        return null
      }

      return elements.map((path) =>
        deleteProperties('always', path, [PP.create('style', 'display')]),
      )
    },
  },
]

export const setPropFillStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  axis: Axis,
  value: 'default' | number,
  otherAxisSetToFill: boolean,
): Array<InspectorStrategy> => [
  fillContainerStrategyFlexParent(metadata, elementPaths, axis, value),
  fillContainerStrategyFlow(metadata, elementPaths, axis, value, otherAxisSetToFill),
]

export const setPropFixedSizeStrategies = (
  whenToRun: WhenToRun,
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  axis: Axis,
  value: CSSNumber,
): Array<InspectorStrategy> => [
  fixedSizeBasicStrategy(whenToRun, metadata, elementPaths, axis, value),
]

export const setPropHugStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  pathTrees: ElementPathTrees,
  axis: Axis,
): Array<InspectorStrategy> => [
  hugContentsGridStrategy(metadata, elementPaths, axis),
  hugContentsBasicStrategy(metadata, elementPaths, pathTrees, axis),
]

export const setPropHugAbsoluteStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
): Array<InspectorStrategy> => [
  hugContentsAbsoluteStrategy(metadata, elementPaths, pathTrees, allElementProps),
]

export const setSpacingModeSpaceBetweenStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
): Array<InspectorStrategy> => [setSpacingModeSpaceBetween(metadata, elementPaths)]

export const setSpacingModePackedStrategies = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
): Array<InspectorStrategy> => [setSpacingModePacked(metadata, elementPaths)]
