import * as PP from '../../../core/shared/property-path'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  Axis,
  filterKeepFlexContainers,
  FlexAlignment,
  flexChildProps,
  FlexJustifyContent,
  pruneFlexPropsCommands,
  sizeToVisualDimensions,
  widthHeightFromAxis,
} from '../inspector-common'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { CSSNumber, FlexDirection } from '../common/css-utils'
import { removeFlexConvertToAbsolute } from './remove-flex-convert-to-absolute-strategy'
import { InspectorStrategy } from './inspector-strategy'
import { WhenToRun } from '../../../components/canvas/commands/commands'
import { hugContentsBasicStrategy } from './hug-contents-basic-strategy'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import { fillContainerStrategyBasic } from './fill-container-basic-strategy'
import { setSpacingModePacked, setSpacingModeSpaceBetween } from './spacing-mode-strategies'
import { convertLayoutToFlexCommands } from '../../common/shared-strategies/convert-to-flex-strategy'

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
  {
    name: 'Set flex direction',
    strategy: (metadata, elementPaths) => {
      const elements = filterKeepFlexContainers(metadata, elementPaths)

      if (elements.length === 0) {
        return null
      }

      return elements.flatMap((path) => [
        setProperty('always', path, PP.create('style', 'flexDirection'), flexDirection),
        ...MetadataUtils.getChildrenPaths(metadata, path).flatMap((child) => [
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
      return convertLayoutToFlexCommands(metadata, elementPaths)
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
): Array<InspectorStrategy> => [fillContainerStrategyBasic(axis, value, otherAxisSetToFill)]

export const setPropFixedStrategies = (
  whenToRun: WhenToRun,
  axis: Axis,
  value: CSSNumber,
): Array<InspectorStrategy> => [
  {
    name: 'Set to Fixed',
    strategy: (metadata, elementPaths) => {
      if (elementPaths.length === 0) {
        return null
      }

      return elementPaths.map((path) => {
        const parentFlexDirection =
          MetadataUtils.findElementByElementPath(metadata, path)?.specialSizeMeasurements
            .parentFlexDirection ?? null

        return setCssLengthProperty(
          whenToRun,
          path,
          PP.create('style', widthHeightFromAxis(axis)),
          setExplicitCssValue(value),
          parentFlexDirection,
        )
      })
    },
  },
]

export const setPropHugStrategies = (axis: Axis): Array<InspectorStrategy> => [
  hugContentsBasicStrategy(axis),
]

export const setSpacingModeSpaceBetweenStrategies: Array<InspectorStrategy> = [
  setSpacingModeSpaceBetween,
]

export const setSpacingModePackedStrategies: Array<InspectorStrategy> = [setSpacingModePacked]
