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
import { hugContentsBasicStrategy } from './hug-contents-basic-strategy'
import {
  fillContainerStrategyFlexParent,
  fillContainerStrategyFlow,
} from './fill-container-basic-strategy'
import { setSpacingModePacked, setSpacingModeSpaceBetween } from './spacing-mode-strategies'
import { convertLayoutToFlexCommands } from '../../common/shared-strategies/convert-to-flex-strategy'
import { fixedSizeBasicStrategy } from './fixed-size-basic-strategy'
import { setFlexDirectionSwapAxes } from './change-flex-direction-swap-axes'
import { fixedEdgeBasicStrategy } from './fixed-edge-basic-strategy'
import type { LayoutEdgeProp } from '../../../core/layout/layout-helpers-new'

export const setFlexAlignStrategies = (flexAlignment: FlexAlignment): Array<InspectorStrategy> => [
  {
    name: 'Set flex-align',
    strategy: (metadata, elementPaths) => {
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
  justifyContent: FlexJustifyContent,
): Array<InspectorStrategy> => [
  {
    name: 'Set justify-content',
    strategy: (metadata, elementPaths) => {
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
  flexAlignment: FlexAlignment,
  justifyContent: FlexJustifyContent,
): Array<InspectorStrategy> => [
  {
    name: 'Set flex-align and justify-content',
    strategy: (metadata, elementPaths) => {
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

export const removeFlexDirectionStrategies = (): Array<InspectorStrategy> => [
  {
    name: 'Unset flex direction',
    strategy: (metadata, elementPaths) => {
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
  flexDirection: FlexDirection,
): Array<InspectorStrategy> => [
  setFlexDirectionSwapAxes(flexDirection),
  {
    name: 'Set flex direction',
    strategy: (metadata, elementPaths, pathTrees) => {
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

export const addFlexLayoutStrategies: Array<InspectorStrategy> = [
  {
    name: 'Add flex layout',
    strategy: (metadata, elementPaths, elementPathTree, allElementProps) => {
      return convertLayoutToFlexCommands(metadata, elementPathTree, elementPaths, allElementProps)
    },
  },
]

export const removeFlexLayoutStrategies: Array<InspectorStrategy> = [
  removeFlexConvertToAbsolute,
  {
    name: 'Remove flex layout',
    strategy: (metadata, elementPaths) => {
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
  axis: Axis,
  value: 'default' | number,
  otherAxisSetToFill: boolean,
): Array<InspectorStrategy> => [
  fillContainerStrategyFlexParent(axis, value),
  fillContainerStrategyFlow(axis, value, otherAxisSetToFill),
]

export const setPropFixedSizeStrategies = (
  whenToRun: WhenToRun,
  axis: Axis,
  value: CSSNumber,
): Array<InspectorStrategy> => [fixedSizeBasicStrategy(whenToRun, axis, value)]

export function setPropFixedEdgeStrategies(
  whenToRun: WhenToRun,
  edge: LayoutEdgeProp,
  value: CSSNumber,
): Array<InspectorStrategy> {
  return [fixedEdgeBasicStrategy(whenToRun, edge, value)]
}

export const setPropHugStrategies = (axis: Axis): Array<InspectorStrategy> => [
  hugContentsBasicStrategy(axis),
]

export const setSpacingModeSpaceBetweenStrategies: Array<InspectorStrategy> = [
  setSpacingModeSpaceBetween,
]

export const setSpacingModePackedStrategies: Array<InspectorStrategy> = [setSpacingModePacked]
