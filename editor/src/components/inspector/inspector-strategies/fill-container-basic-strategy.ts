import * as PP from '../../../core/shared/property-path'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { clamp } from '../../../core/shared/math-utils'
import { setProperty } from '../../canvas/commands/set-property-command'
import type { FlexDirection } from '../common/css-utils'
import { cssNumber, printCSSNumber } from '../common/css-utils'
import type { Axis } from '../inspector-common'
import {
  fillContainerApplicable,
  nukeAllAbsolutePositioningPropsCommands,
  nukePositioningPropsForAxisCommand,
  widthHeightFromAxis,
  detectParentFlexDirection,
  nukeSizingPropsForAxisCommand,
  nullOrNonEmpty,
  setParentToFixedIfHugCommands,
} from '../inspector-common'
import type { InspectorStrategy } from './inspector-strategy'
import {
  setCssLengthProperty,
  setExplicitCssValue,
} from '../../canvas/commands/set-css-length-command'
import {
  groupErrorToastCommand,
  maybeInvalidGroupState,
} from '../../canvas/canvas-strategies/strategies/group-helpers'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../components/editor/store/editor-state'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'

export const fillContainerStrategyFlow = (
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPaths: ElementPath[],
  axis: Axis,
  value: 'default' | number,
  otherAxisSetToFill: boolean,
): InspectorStrategy => ({
  name: 'Set to Fill Container',
  strategy: () => {
    const elements = elementPaths.filter((elementPath) =>
      fillContainerApplicable(metadata, elementPath),
    )

    if (elements.length === 0) {
      return null
    }

    const invalidGroupState = maybeInvalidGroupState(elements, metadata, {
      onGroup: () => 'group-has-percentage-pins',
      onGroupChild: (path) => {
        return 'child-has-percentage-pins'
      },
    })
    if (invalidGroupState != null) {
      return [groupErrorToastCommand(invalidGroupState)]
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
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPaths: ElementPath[],
  axis: Axis,
  value: 'default' | number,
  overrides: Partial<FillContainerStrategyFlexParentOverrides> = {},
): InspectorStrategy => ({
  name: 'Set to Fill Container, in flex layout',
  strategy: () => {
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
