import * as PP from '../../../core/shared/property-path'
import * as EP from '../../../core/shared/element-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import {
  CanvasRectangle,
  Rectangle,
  SimpleRectangle,
  canvasRectangle,
  clamp,
  zeroRectIfNullOrInfinity,
} from '../../../core/shared/math-utils'
import { setProperty } from '../../canvas/commands/set-property-command'
import { cssNumber, FlexDirection, printCSSNumber } from '../common/css-utils'
import {
  fillContainerApplicable,
  nukeAllAbsolutePositioningPropsCommands,
  nukePositioningPropsForAxisCommand,
  widthHeightFromAxis,
  detectParentFlexDirection,
  nukeSizingPropsForAxisCommand,
  Axis,
  nullOrNonEmpty,
  setParentToFixedIfHugCommands,
  predictElementSize,
} from '../inspector-common'
import { InspectorStrategy } from './inspector-strategy'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { ElementPath } from '../../../core/shared/project-file-types'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { pushIntendedBoundsAndUpdateGroups } from '../../canvas/commands/push-intended-bounds-and-update-groups-command'

export const fillContainerStrategyFlow = (
  axis: Axis,
  value: 'default' | number,
  otherAxisSetToFill: boolean,
): InspectorStrategy => ({
  name: 'Set tp Fill Container',
  strategy: (metadata, elementPaths) => {
    const elements = elementPaths.filter((elementPath) =>
      fillContainerApplicable(metadata, elementPath),
    )

    if (elements.length === 0) {
      return null
    }

    return elements.flatMap((path) => {
      const instance = MetadataUtils.findElementByElementPath(metadata, path)
      const checkedValue =
        value === 'default' ? cssNumber(100, '%') : cssNumber(clamp(0, 100, value), '%')
      const nukePositioningCommands = otherAxisSetToFill
        ? nukeAllAbsolutePositioningPropsCommands(path)
        : [nukePositioningPropsForAxisCommand(axis, path)]

      const propToChange = widthHeightFromAxis(axis)
      const predictedElementSize = predictElementSize(metadata, path, propToChange, checkedValue)

      return [
        ...nukePositioningCommands,
        ...setParentToFixedIfHugCommands(axis, metadata, path),
        setCssLengthProperty(
          'always',
          path,
          PP.create('style', propToChange),
          setExplicitCssValue(checkedValue),
          instance?.specialSizeMeasurements.parentFlexDirection ?? null,
        ),
        pushIntendedBoundsAndUpdateGroups([{ target: path, frame: predictedElementSize }]), // TODO before merge find code path where this runs
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

        const propToChange = widthHeightFromAxis(axis)
        const predictedElementSize = predictElementSize(metadata, path, propToChange, checkedValue)

        return [
          ...setParentToFixedIfHugCommands(axis, metadata, path),
          setCssLengthProperty(
            'always',
            path,
            PP.create('style', propToChange),
            setExplicitCssValue(checkedValue),
            flexDirection,
          ),
          pushIntendedBoundsAndUpdateGroups([{ target: path, frame: predictedElementSize }]), // TODO before merge find code path where this runs
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
