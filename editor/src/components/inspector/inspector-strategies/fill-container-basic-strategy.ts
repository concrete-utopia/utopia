import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import { clamp } from '../../../core/shared/math-utils'
import * as PP from '../../../core/shared/property-path'
import {
  groupErrorToastCommand,
  maybeGroupWithoutFixedSizeForFill,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import { setProperty } from '../../canvas/commands/set-property-command'
import type { FlexDirection } from '../common/css-utils'
import { cssNumber, printCSSNumber } from '../common/css-utils'
import type { Axis } from '../inspector-common'
import {
  detectParentFlexDirection,
  fillContainerApplicable,
  nukeAllAbsolutePositioningPropsCommands,
  nukePositioningPropsForAxisCommand,
  nukeSizingPropsForAxisCommand,
  nullOrNonEmpty,
  setParentToFixedIfHugCommands,
  widthHeightFromAxis,
} from '../inspector-common'
import { maybeInvalidGroupStates } from './inspector-strategies'
import type { InspectorStrategy } from './inspector-strategy'

export const fillContainerStrategyFlow = (
  axis: Axis,
  value: 'default' | number,
  otherAxisSetToFill: boolean,
): InspectorStrategy => ({
  name: 'Set to Fill Container',
  strategy: (metadata, elementPaths) => {
    const elements = elementPaths.filter((elementPath) =>
      fillContainerApplicable(metadata, elementPath),
    )

    if (elements.length === 0) {
      return null
    }

    const maybeInvalidGroupState = maybeInvalidGroupStates(elements, metadata, (path) => {
      const group = MetadataUtils.getJSXElementFromMetadata(metadata, EP.parentPath(path))
      return maybeGroupWithoutFixedSizeForFill(group) ?? null
    })
    if (maybeInvalidGroupState != null) {
      return [groupErrorToastCommand(maybeInvalidGroupState)]
    }

    return elements.flatMap((path) => {
      const instance = MetadataUtils.findElementByElementPath(metadata, path)
      const checkedValue =
        value === 'default' ? cssNumber(100, '%') : cssNumber(clamp(0, 100, value), '%')
      const nukePositioningCommands = otherAxisSetToFill
        ? nukeAllAbsolutePositioningPropsCommands(path)
        : [nukePositioningPropsForAxisCommand(axis, path)]
      return [
        ...nukePositioningCommands,
        ...setParentToFixedIfHugCommands(axis, metadata, path),
        setCssLengthProperty(
          'always',
          path,
          PP.create('style', widthHeightFromAxis(axis)),
          setExplicitCssValue(checkedValue),
          instance?.specialSizeMeasurements.parentFlexDirection ?? null,
        ),
      ]
    })
  },
})

export interface FillContainerStrategyFlexParentOverrides {
  forceFlexDirectionForParent: FlexDirection
}

export const fillContainerStrategyFlexParent = (
  axis: Axis,
  value: 'default' | number,
  overrides: Partial<FillContainerStrategyFlexParentOverrides> = {},
): InspectorStrategy => ({
  name: 'Set to Fill Container, in flex layout',
  strategy: (metadata, elementPaths) => {
    const elements = elementPaths.filter(
      (path) =>
        fillContainerApplicable(metadata, path) &&
        MetadataUtils.isParentFlexLayoutedContainerForElement(
          MetadataUtils.findElementByElementPath(metadata, path),
        ),
    )

    if (elements.length === 0) {
      return null
    }

    const commands = elements.flatMap((path) => {
      const flexDirection =
        overrides.forceFlexDirectionForParent ?? detectParentFlexDirection(metadata, path) ?? 'row'

      if (
        (flexDirection.startsWith('row') && axis === 'vertical') ||
        (flexDirection.startsWith('column') && axis === 'horizontal')
      ) {
        const checkedValue =
          value === 'default' ? cssNumber(100, '%') : cssNumber(clamp(0, 100, value), '%')
        return [
          ...setParentToFixedIfHugCommands(axis, metadata, path),
          setCssLengthProperty(
            'always',
            path,
            PP.create('style', widthHeightFromAxis(axis)),
            setExplicitCssValue(checkedValue),
            flexDirection,
          ),
        ]
      }

      const checkedValue =
        value === 'default' ? cssNumber(1, null) : cssNumber(clamp(0, Infinity, value), null)

      return [
        ...nukeAllAbsolutePositioningPropsCommands(path),
        ...setParentToFixedIfHugCommands(axis, metadata, path),
        nukeSizingPropsForAxisCommand(axis, path),
        setProperty(
          'always',
          path,
          PP.create('style', 'flexGrow'),
          printCSSNumber(checkedValue, null),
        ),
      ]
    })

    return nullOrNonEmpty(commands)
  },
})
