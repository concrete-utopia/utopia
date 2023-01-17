import * as PP from '../../../core/shared/property-path'
import { setProperty } from '../../canvas/commands/set-property-command'
import {
  Axis,
  convertWidthToFlexGrow,
  detectFlexDirectionOne,
  fillContainerApplicable,
  filterKeepFlexContainers,
  FlexAlignment,
  flexChildProps,
  FlexJustifyContent,
  hugContentsApplicableForContainer,
  pruneFlexPropsCommands,
  sizeToVisualDimensions,
  widthHeightFromAxis,
} from '../inspector-common'
import * as EP from '../../../core/shared/element-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { assertNever } from '../../../core/shared/utils'
import { CanvasCommand } from '../../canvas/commands/commands'
import { deleteProperties } from '../../canvas/commands/delete-properties-command'
import { CSSNumber, FlexDirection, printCSSNumber } from '../common/css-utils'
import { removeFlexConvertToAbsolute } from './remove-flex-convert-to-absolute-strategy'
import { hugContentsTextStrategy } from './hug-contents-text'

export type InspectorStrategy = (
  metadata: ElementInstanceMetadataMap,
  selectedElementPaths: Array<ElementPath>,
) => Array<CanvasCommand> | null

export const setFlexAlignJustifyContentStrategies = (
  flexAlignment: FlexAlignment,
  justifyContent: FlexJustifyContent,
): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = filterKeepFlexContainers(metadata, elementPaths)

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => [
      setProperty('always', path, PP.create(['style', 'alignItems']), flexAlignment),
      setProperty('always', path, PP.create(['style', 'justifyContent']), justifyContent),
    ])
  },
]

export const removeFlexDirectionStrategies = (): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = filterKeepFlexContainers(metadata, elementPaths)

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      deleteProperties('always', path, [PP.create(['style', 'flexDirection'])]),
    )
  },
]

export const updateFlexDirectionStrategies = (
  flexDirection: FlexDirection,
): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
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
]

export const addFlexLayoutStrategies: Array<InspectorStrategy> = [
  (metadata, elementPaths) => {
    return elementPaths.flatMap((path) => [
      setProperty('always', path, PP.create(['style', 'display']), 'flex'),
      ...MetadataUtils.getChildrenPaths(metadata, path).flatMap((child) =>
        convertWidthToFlexGrow(metadata, child, path),
      ),
    ])
  },
]

export const removeFlexLayoutStrategies: Array<InspectorStrategy> = [
  removeFlexConvertToAbsolute,
  (metadata, elementPaths) => {
    const elements = filterKeepFlexContainers(metadata, elementPaths)

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      deleteProperties('always', path, [PP.create(['style', 'display'])]),
    )
  },
]

export const setPropFillStrategies = (axis: Axis): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    const elements = elementPaths.filter(fillContainerApplicable)

    if (elements.length === 0) {
      return null
    }

    const nukePropsCommand = (path: ElementPath) => {
      switch (axis) {
        case 'horizontal':
          return deleteProperties('always', path, [
            PP.create(['style', 'width']),
            PP.create(['style', 'minWidth']),
            PP.create(['style', 'maxWidth']),
          ])
        case 'vertical':
          return deleteProperties('always', path, [
            PP.create(['style', 'height']),
            PP.create(['style', 'minHeight']),
            PP.create(['style', 'maxHeight']),
          ])
        default:
          assertNever(axis)
      }
    }

    return elements.flatMap((path) => {
      const parentInstance = MetadataUtils.findElementByElementPath(metadata, EP.parentPath(path))
      if (!MetadataUtils.isFlexLayoutedContainer(parentInstance)) {
        return [
          nukePropsCommand(path),
          setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), '100%'),
        ]
      }

      const flexDirection = detectFlexDirectionOne(metadata, EP.parentPath(path)) ?? 'row'

      if (
        (flexDirection.startsWith('row') && axis === 'vertical') ||
        (flexDirection.startsWith('column') && axis === 'horizontal')
      ) {
        return [
          nukePropsCommand(path),
          setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), '100%'),
        ]
      }

      return [
        nukePropsCommand(path),
        setProperty('always', path, PP.create(['style', 'flexGrow']), '1'),
      ]
    })
  },
]

export const setPropFixedStrategies = (axis: Axis, value: CSSNumber): Array<InspectorStrategy> => [
  (metadata, elementPaths) => {
    if (elementPaths.length === 0) {
      return null
    }

    return elementPaths.map((path) =>
      setProperty(
        'always',
        path,
        PP.create(['style', widthHeightFromAxis(axis)]),
        printCSSNumber(value, null),
      ),
    )
  },
]

export const setPropHugStrategies = (axis: Axis): Array<InspectorStrategy> => [
  hugContentsTextStrategy(axis),
  (metadata, elementPaths) => {
    const elements = elementPaths.filter((path) =>
      hugContentsApplicableForContainer(metadata, path),
    )

    if (elements.length === 0) {
      return null
    }

    return elements.map((path) =>
      setProperty('always', path, PP.create(['style', widthHeightFromAxis(axis)]), 'min-content'),
    )
  },
]
