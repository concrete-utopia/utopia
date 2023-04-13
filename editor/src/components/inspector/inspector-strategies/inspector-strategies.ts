import * as PP from '../../../core/shared/property-path'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  Axis,
  filterKeepFlexContainers,
  FlexAlignment,
  flexChildProps,
  FlexJustifyContent,
  nullOrNonEmpty,
  pruneFlexPropsCommands,
  sizeToVisualDimensions,
} from '../inspector-common'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { CSSNumber, FlexDirection } from '../common/css-utils'
import { removeFlexConvertToAbsolute } from './remove-flex-convert-to-absolute-strategy'
import { InspectorStrategy } from './inspector-strategy'
import { WhenToRun } from '../../../components/canvas/commands/commands'
import { hugContentsBasicStrategy } from './hug-contents-basic-strategy'
import {
  fillContainerStrategyFlexParent,
  fillContainerStrategyFlow,
} from './fill-container-basic-strategy'
import { setSpacingModePacked, setSpacingModeSpaceBetween } from './spacing-mode-strategies'
import {
  convertLayoutToFlexCommands,
  ifElementIsFragmentFirstConvertItToFrame,
} from '../../common/shared-strategies/convert-to-flex-strategy'
import { fixedSizeBasicStrategy } from './fixed-size-basic-strategy'
import { setFlexDirectionSwapAxes } from './change-flex-direction-swap-axes'
import { showToastCommand } from '../../canvas/commands/show-toast-command'
import { flattenSelection } from '../../canvas/canvas-strategies/strategies/shared-move-strategies-helpers'

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
    strategy: (metadata, elementPaths) => {
      const elements = filterKeepFlexContainers(metadata, elementPaths)

      if (elements.length === 0) {
        return null
      }

      return elements.flatMap((path) => [
        setProperty('always', path, PP.create('style', 'flexDirection'), flexDirection),
        ...MetadataUtils.getChildrenPathsUnordered(metadata, path).flatMap((child) => [
          ...pruneFlexPropsCommands(flexChildProps, child),
          ...sizeToVisualDimensions(metadata, child),
        ]),
      ])
    },
  },
]

export const addFlexLayoutStrategies: Array<InspectorStrategy> = [
  {
    name: 'Add flex layout',
    strategy: (metadata, elementPaths) => {
      return nullOrNonEmpty(convertLayoutToFlexCommands(metadata, elementPaths))
    },
  },
  {
    name: 'Add flex layout (basic)',
    strategy: (metadata, elementPaths) => {
      return flattenSelection(elementPaths).flatMap((elementPath) => [
        ...ifElementIsFragmentFirstConvertItToFrame(metadata, elementPath),
        setProperty('always', elementPath, PP.create('style', 'display'), 'flex'),
      ])
    },
  },
  {
    name: 'Add flex layout (apology)',
    strategy: () => [
      showToastCommand(
        'Cannot be converted to Flex yet',
        'NOTICE',
        'cannot-convert-children-to-flex',
      ),
    ],
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

export const setPropFixedStrategies = (
  whenToRun: WhenToRun,
  axis: Axis,
  value: CSSNumber,
): Array<InspectorStrategy> => [fixedSizeBasicStrategy(whenToRun, axis, value)]

export const setPropHugStrategies = (axis: Axis): Array<InspectorStrategy> => [
  hugContentsBasicStrategy(axis),
]

export const setSpacingModeSpaceBetweenStrategies: Array<InspectorStrategy> = [
  setSpacingModeSpaceBetween,
]

export const setSpacingModePackedStrategies: Array<InspectorStrategy> = [setSpacingModePacked]
