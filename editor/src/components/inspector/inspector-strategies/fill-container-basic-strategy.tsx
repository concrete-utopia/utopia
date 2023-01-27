import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { clamp } from '../../../core/shared/math-utils'
import { setProperty } from '../../canvas/commands/set-property-command'
import { cssNumber, printCSSNumber } from '../common/css-utils'
import {
  fillContainerApplicable,
  nukeAllAbsolutePositioningPropsCommands,
  nukePositioningPropsForAxisCommand,
  widthHeightFromAxis,
  detectParentFlexDirection,
  nukeSizingPropsForAxisCommand,
  Axis,
} from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'

export const fillContainerStrategyBasic = (
  axis: Axis,
  value: 'default' | number,
  otherAxisSetToFill: boolean,
): InspectorStrategy => ({
  name: 'Set to Fill Container',
  strategy: (metadata, elementPaths) => {
    const elements = elementPaths.filter(fillContainerApplicable)

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => {
      const instance = MetadataUtils.findElementByElementPath(metadata, path)
      if (!MetadataUtils.isParentFlexLayoutedContainerForElement(instance)) {
        const checkedValue =
          value === 'default' ? cssNumber(100, '%') : cssNumber(clamp(0, 100, value), '%')
        const nukePositioningCommands = otherAxisSetToFill
          ? nukeAllAbsolutePositioningPropsCommands(path)
          : [nukePositioningPropsForAxisCommand(axis, path)]
        return [
          ...nukePositioningCommands,
          setCssLengthProperty(
            'always',
            path,
            PP.create('style', widthHeightFromAxis(axis)),
            setExplicitCssValue(checkedValue),
            instance?.specialSizeMeasurements.parentFlexDirection ?? null,
          ),
        ]
      }

      const flexDirection = detectParentFlexDirection(metadata, path) ?? 'row'

      if (
        (flexDirection.startsWith('row') && axis === 'vertical') ||
        (flexDirection.startsWith('column') && axis === 'horizontal')
      ) {
        const checkedValue =
          value === 'default' ? cssNumber(100, '%') : cssNumber(clamp(0, 100, value), '%')
        return [
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
        nukeSizingPropsForAxisCommand(axis, path),
        setProperty(
          'always',
          path,
          PP.create('style', 'flexGrow'),
          printCSSNumber(checkedValue, null),
        ),
      ]
    })
  },
})
