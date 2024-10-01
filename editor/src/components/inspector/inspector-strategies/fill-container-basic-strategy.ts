import * as PP from '../../../core/shared/property-path'
import { getSimpleAttributeAtPath, MetadataUtils } from '../../../core/model/element-metadata-utils'
import { clamp } from '../../../core/shared/math-utils'
import {
  propertyToDelete,
  propertyToSet,
  setProperty,
  updateBulkProperties,
} from '../../canvas/commands/set-property-command'
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
  removeAlignJustifySelf,
  styleP,
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
import {
  isJSXElement,
  type ElementInstanceMetadataMap,
} from '../../../core/shared/element-template'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { foldEither, defaultEither, right, isLeft } from '../../../core/shared/either'
import { parseString } from '../../../utils/value-parser-utils'

export const fillContainerStrategyFlow = (
  metadata: ElementInstanceMetadataMap,
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
      const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)

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
          ...removeAlignJustifySelf(axis, elementMetadata),
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
        ...removeAlignJustifySelf(axis, elementMetadata),
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

export const fillContainerStrategyGridParent = (
  metadata: ElementInstanceMetadataMap,
  elementPaths: ElementPath[],
  axis: Axis,
): InspectorStrategy => ({
  name: 'Set to Fill Container, in grid layout',
  strategy: () => {
    const elements = elementPaths.filter(
      (path) => fillContainerApplicable(metadata, path) && MetadataUtils.isGridCell(metadata, path),
    )

    if (elements.length === 0) {
      return null
    }

    const commands = elements.flatMap((path) => {
      const element = MetadataUtils.findElementByElementPath(metadata, path)
      if (element == null || isLeft(element.element) || !isJSXElement(element.element.value)) {
        return []
      }

      const alignSelf = foldEither(
        () => null,
        (value) => defaultEither(null, parseString(value)),
        getSimpleAttributeAtPath(right(element.element.value.props), styleP('alignSelf')),
      )

      const justifySelf = foldEither(
        () => null,
        (value) => defaultEither(null, parseString(value)),
        getSimpleAttributeAtPath(right(element.element.value.props), styleP('justifySelf')),
      )

      let updates = [
        propertyToSet(styleP(axis === 'horizontal' ? 'alignSelf' : 'justifySelf'), 'stretch'),
      ]

      // delete the opposite side value (justify <> align) if not set to stretch
      if (axis === 'vertical' && alignSelf !== 'stretch') {
        updates.push(propertyToDelete(styleP('alignSelf')))
      } else if (axis === 'horizontal' && justifySelf !== 'stretch') {
        updates.push(propertyToDelete(styleP('justifySelf')))
      }

      return [
        ...nukeAllAbsolutePositioningPropsCommands(path),
        ...setParentToFixedIfHugCommands(axis, metadata, path),
        nukeSizingPropsForAxisCommand(axis, path),
        updateBulkProperties('always', path, updates),
      ]
    })

    return nullOrNonEmpty(commands)
  },
})
