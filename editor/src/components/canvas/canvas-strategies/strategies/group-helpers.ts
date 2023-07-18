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

export type GroupState = 'valid' | InvalidGroupState

export type InvalidGroupState =
  | 'child-not-position-absolute'
  | 'child-has-percentage-pins-without-group-size'
  | 'child-missing-props'
  | 'group-has-percentage-pins'
  | 'unknown'

export function isInvalidGroupState(s: GroupState | null): s is InvalidGroupState {
  return s !== 'valid'
}

export function invalidGroupStateToString(s: InvalidGroupState): string {
  switch (s) {
    // group state
    case 'group-has-percentage-pins':
      return 'Group has % pins'

    // children state
    case 'child-not-position-absolute':
      return 'Group children have non-absolute position'
    case 'child-has-percentage-pins-without-group-size':
      return 'Group children have % pins, but group has no size'
    case 'child-missing-props':
      return 'Missing props'

    // fallback
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

export function getGroupState(path: ElementPath, metadata: ElementInstanceMetadataMap): GroupState {
  const group = MetadataUtils.getJSXElementFromMetadata(metadata, path)

  if (group == null) {
    return 'unknown'
  } else if (elementHasPercentagePins(group)) {
    return 'group-has-percentage-pins'
  } else {
    const groupHasExplicitSize = checkGroupHasExplicitSize(group)
    return (
      MetadataUtils.getChildrenUnordered(metadata, path)
        .map((child) => MetadataUtils.findElementByElementPath(metadata, child.elementPath))
        .map((child) => getGroupChildState(child, groupHasExplicitSize))
        .find(isInvalidGroupState) ?? 'valid'
    )
  }
}

function getGroupChildState(
  elementMetadata: ElementInstanceMetadata | null,
  groupHasExplicitSize: boolean,
): GroupState {
  if (elementMetadata == null) {
    return 'unknown'
  }

  const jsxElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(elementMetadata)

  if (jsxElement?.props == null) {
    return 'child-missing-props'
  } else if (!MetadataUtils.isPositionAbsolute(elementMetadata)) {
    return 'child-not-position-absolute'
  } else if (!groupHasExplicitSize && elementHasPercentagePins(jsxElement)) {
    return 'child-has-percentage-pins-without-group-size'
  } else {
    return 'valid'
  }
}
