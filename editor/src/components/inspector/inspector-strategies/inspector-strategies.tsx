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
  nukeSizingPropsForAxisCommand,
  pruneFlexPropsCommands,
  sizeToVisualDimensions,
  widthHeightFromAxis,
} from '../inspector-common'
import * as EP from '../../../core/shared/element-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { CSSNumber, FlexDirection, printCSSNumber } from '../common/css-utils'
import { removeFlexConvertToAbsolute } from './remove-flex-convert-to-absolute-strategy'
import { InspectorStrategy } from './inspector-strategy'
import { WhenToRun } from '../../../components/canvas/commands/commands'
import { assertNever } from '../../../core/shared/utils'
import { PropertyPath } from 'src/core/shared/project-file-types'
import { hugContentsBasicStrategy } from './hug-contents-basic-strategy'

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

export const setPropFillStrategies = (axis: Axis): Array<InspectorStrategy> => [
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
            setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), '100%'),
          ]
        }

        return [
          nukeSizingPropsForAxisCommand(axis, path),
          setProperty('always', path, PP.create(['style', 'flexGrow']), '1'),
        ]
      })
    },
  },
]

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

      return elementPaths.flatMap((path) => {
        // Only delete these properties when this is a flex child.
        let propertiesToDelete: Array<PropertyPath> = []
        const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)
        if (
          elementMetadata != null &&
          elementMetadata.specialSizeMeasurements.parentLayoutSystem === 'flex'
        ) {
          switch (axis) {
            case 'horizontal':
              propertiesToDelete = [
                PP.create(['style', 'minWidth']),
                PP.create(['style', 'maxWidth']),
              ]
              break
            case 'vertical':
              propertiesToDelete = [
                PP.create(['style', 'minHeight']),
                PP.create(['style', 'maxHeight']),
              ]
              break
            default:
              assertNever(axis)
          }
        }

        switch (axis) {
          case 'horizontal':
            return [
              deleteProperties(whenToRun, path, propertiesToDelete),
              setProperty(
                whenToRun,
                path,
                PP.create(['style', 'width']),
                printCSSNumber(value, null),
              ),
            ]
          case 'vertical':
            return [
              deleteProperties(whenToRun, path, propertiesToDelete),
              setProperty(
                whenToRun,
                path,
                PP.create(['style', 'height']),
                printCSSNumber(value, null),
              ),
            ]
          default:
            assertNever(axis)
        }
      })
    },
  },
]

export const setPropHugStrategies = (axis: Axis): Array<InspectorStrategy> => [
  hugContentsBasicStrategy(axis),
]
