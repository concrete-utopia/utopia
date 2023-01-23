import * as PP from '../../../core/shared/property-path'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  Axis,
  convertWidthToFlexGrow,
  detectParentFlexDirection,
  fillContainerApplicable,
  filterKeepFlexContainers,
  FlexAlignment,
  flexChildProps,
  FlexJustifyContent,
  hugContentsApplicableForContainer,
  nukeAllAbsolutePositioningPropsCommand,
  nukePositioningPropsForAxisCommand,
  nukeSizingPropsForAxisCommand,
  pruneFlexPropsCommands,
  removeAbsolutePositioningPropCommand,
  sizeToVisualDimensions,
  widthHeightFromAxis,
} from '../inspector-common'
import * as EP from '../../../core/shared/element-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { CSSNumber, FlexDirection, printCSSNumber } from '../common/css-utils'
import { removeFlexConvertToAbsolute } from './remove-flex-convert-to-absolute-strategy'
import { hugContentsTextStrategy } from './hug-contents-text'
import { InspectorStrategy } from './inspector-strategy'

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
        setProperty('always', path, PP.create(['style', 'alignItems']), flexAlignment),
        setProperty('always', path, PP.create(['style', 'justifyContent']), justifyContent),
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
        deleteProperties('always', path, [PP.create(['style', 'flexDirection'])]),
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
        setProperty('always', path, PP.create(['style', 'flexDirection']), flexDirection),
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
      return elementPaths.flatMap((path) => [
        setProperty('always', path, PP.create(['style', 'display']), 'flex'),
        ...MetadataUtils.getChildrenPaths(metadata, path).flatMap((child) =>
          convertWidthToFlexGrow(metadata, child),
        ),
      ])
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
        deleteProperties('always', path, [PP.create(['style', 'display'])]),
      )
    },
  },
]

export const setPropFillStrategies = (
  axis: Axis,
  otherAxisFilled: boolean,
): Array<InspectorStrategy> => [
  {
    name: 'Set to Fill Container',
    strategy: (metadata, elementPaths) => {
      const elements = elementPaths.filter(fillContainerApplicable)

      if (elements.length === 0) {
        return null
      }

      return elements.flatMap((path) => {
        const parentInstance = MetadataUtils.findElementByElementPath(metadata, EP.parentPath(path))
        if (!MetadataUtils.isFlexLayoutedContainer(parentInstance)) {
          return [
            nukeSizingPropsForAxisCommand(axis, path),
            otherAxisFilled
              ? nukeAllAbsolutePositioningPropsCommand(path)
              : nukePositioningPropsForAxisCommand(axis, path),
            setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), '100%'),
          ]
        }

        const flexDirection = detectParentFlexDirection(metadata, path) ?? 'row'

        if (
          (flexDirection.startsWith('row') && axis === 'vertical') ||
          (flexDirection.startsWith('column') && axis === 'horizontal')
        ) {
          return [
            nukeSizingPropsForAxisCommand(axis, path),
            nukeAllAbsolutePositioningPropsCommand(path),
            setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), '100%'),
          ]
        }

        return [
          nukeSizingPropsForAxisCommand(axis, path),
          nukeAllAbsolutePositioningPropsCommand(path),
          setProperty('always', path, PP.create(['style', 'flexGrow']), '1'),
        ]
      })
    },
  },
]

export const setPropFixedStrategies = (axis: Axis, value: number): Array<InspectorStrategy> => [
  {
    name: 'Set to Fixed',
    strategy: (metadata, elementPaths) => {
      if (elementPaths.length === 0) {
        return null
      }

      return elementPaths.map((path) =>
        setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), value),
      )
    },
  },
]

export const setPropHugStrategies = (axis: Axis): Array<InspectorStrategy> => [
  hugContentsTextStrategy(axis),
  {
    name: 'Set to Hug',
    strategy: (metadata, elementPaths) => {
      const elements = elementPaths.filter((path) =>
        hugContentsApplicableForContainer(metadata, path),
      )

      if (elements.length === 0) {
        return null
      }

      return elements.flatMap((path) => [
        nukeSizingPropsForAxisCommand(axis, path),
        setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), 'min-content'),
      ])
    },
  },
]
