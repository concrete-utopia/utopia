import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXElement,
} from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import { styleStringInArray } from '../../../../utils/common-constants'
import { isRight, right } from '../../../../core/shared/either'
import { isCSSNumber } from '../../../inspector/common/css-utils'
import { assertNever } from '../../../../core/shared/utils'

export function treatElementAsGroupLike(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): boolean {
  const allChildrenAreAbsolute = MetadataUtils.getChildrenOrdered(metadata, pathTrees, path).every(
    (child) => child.specialSizeMeasurements.position === 'absolute',
  )
  return (
    allChildrenAreAbsolute &&
    MetadataUtils.isGroupAgainstImports(MetadataUtils.findElementByElementPath(metadata, path))
  )
}

export type GroupChildState = 'valid' | InvalidGroupChildState

export type InvalidGroupChildState =
  | 'not-position-absolute'
  | 'percentage-pins-without-group-size'
  | 'missing-props'
  | 'unknown'

export function isInvalidGroupChildState(s: GroupChildState | null): s is InvalidGroupChildState {
  return s !== 'valid'
}

export function invalidGroupChildStateToString(s: InvalidGroupChildState): string {
  switch (s) {
    case 'not-position-absolute':
      return 'Group children have non-absolute position'
    case 'percentage-pins-without-group-size':
      return 'Group children have % pins, but group has no size'
    case 'missing-props':
      return 'Missing props'
    case 'unknown':
      return 'Invalid group'
    default:
      assertNever(s)
  }
}

function checkGroupHasExplicitSize(group: JSXElement): boolean {
  const groupDimensions = [
    getLayoutProperty('width', right(group.props), styleStringInArray),
    getLayoutProperty('height', right(group.props), styleStringInArray),
  ]

  return groupDimensions.every((dimension) => {
    return isRight(dimension) && isCSSNumber(dimension.value)
  })
}

function elementHasPercentagePins(jsxElement: JSXElement): boolean {
  const pins = [
    getLayoutProperty('width', right(jsxElement.props), styleStringInArray),
    getLayoutProperty('height', right(jsxElement.props), styleStringInArray),
    getLayoutProperty('left', right(jsxElement.props), styleStringInArray),
    getLayoutProperty('top', right(jsxElement.props), styleStringInArray),
  ]
  return pins.some((pin) => {
    return isRight(pin) && isCSSNumber(pin.value) && pin.value.unit === '%'
  })
}

function getGroupChildState(
  elementMetadata: ElementInstanceMetadata | null,
  groupHasExplicitSize: boolean,
): GroupChildState {
  if (elementMetadata == null) {
    return 'unknown'
  }

  const jsxElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(elementMetadata)

  if (jsxElement?.props == null) {
    return 'missing-props'
  } else if (!MetadataUtils.isPositionAbsolute(elementMetadata)) {
    return 'not-position-absolute'
  } else if (!groupHasExplicitSize && elementHasPercentagePins(jsxElement)) {
    return 'percentage-pins-without-group-size'
  } else {
    return 'valid'
  }
}

export function getGroupState(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): GroupChildState {
  const group = MetadataUtils.getJSXElementFromMetadata(metadata, path)
  if (group == null) {
    return 'unknown'
  }

  const groupHasExplicitSize = checkGroupHasExplicitSize(group)
  return (
    MetadataUtils.getChildrenUnordered(metadata, path)
      .map((child) => MetadataUtils.findElementByElementPath(metadata, child.elementPath))
      .map((child) => getGroupChildState(child, groupHasExplicitSize))
      .find(isInvalidGroupChildState) ?? 'valid'
  )
}
