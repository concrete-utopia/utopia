import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { StyleLayoutProp } from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft, isRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXElement,
  JSXElementChildren,
  JSXElementWithoutUID,
} from '../../../../core/shared/element-template'
import {
  emptyComments,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxElementWithoutUID,
} from '../../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../../core/shared/project-file-types'
import { importAlias } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { styleStringInArray } from '../../../../utils/common-constants'
import { notice } from '../../../common/notice'
import type { AddToast } from '../../../editor/action-types'
import { showToast } from '../../../editor/actions/action-creators'
import type { CSSNumber } from '../../../inspector/common/css-utils'
import { isCSSNumber } from '../../../inspector/common/css-utils'
import type { ShowToastCommand } from '../../commands/show-toast-command'
import { showToastCommand } from '../../commands/show-toast-command'

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
  | 'child-has-missing-pins'
  | 'group-has-percentage-pins'
  | 'unknown'

export function isInvalidGroupState(s: GroupState | null): s is InvalidGroupState {
  return s !== 'valid'
}

export function invalidGroupStateToString(state: InvalidGroupState): string {
  switch (state) {
    // group state
    case 'group-has-percentage-pins':
      return `Groups shouldn't use % pins`

    // children state
    case 'child-not-position-absolute':
      return 'All children of group should have absolute position'
    case 'child-has-percentage-pins-without-group-size':
      return 'Group needs dimensions to use children with % pins'
    case 'child-has-missing-pins':
      return 'All children of group should have valid pins'

    // fallback
    case 'unknown':
      return 'Invalid group'

    default:
      assertNever(state)
  }
}

export function checkGroupHasExplicitSize(group: JSXElement | null): boolean {
  if (group == null) {
    return false
  }
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

function elementHasValidPins(jsxElement: JSXElement): boolean {
  function getPin(name: StyleLayoutProp): CSSNumber | null {
    const pin = getLayoutProperty(name, right(jsxElement.props), styleStringInArray)
    return isRight(pin) && isCSSNumber(pin.value) ? pin.value : null
  }
  const leftPin = getPin('left')
  const rightPin = getPin('right')
  const topPin = getPin('top')
  const bottomPin = getPin('bottom')
  const width = getPin('width')
  const height = getPin('height')

  if (leftPin != null || rightPin != null) {
    return topPin != null || bottomPin != null || height?.unit === '%'
  } else {
    return !(topPin != null || bottomPin != null) || width?.unit === '%'
  }
}

export function getGroupStateFromJSXElement(
  jsxElement: JSXElement,
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): GroupState {
  return (
    maybeGroupHasPercentagePins(jsxElement) ??
    maybeInvalidGroupChildren(jsxElement, path, metadata) ??
    'valid'
  )
}

export function getGroupState(path: ElementPath, metadata: ElementInstanceMetadataMap): GroupState {
  const group = MetadataUtils.getJSXElementFromMetadata(metadata, path)
  if (group == null) {
    return 'unknown'
  }
  return getGroupStateFromJSXElement(group, path, metadata)
}

function maybeInvalidGroupChildren(
  group: JSXElement,
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): InvalidGroupState | 'valid' {
  const groupHasExplicitSize = checkGroupHasExplicitSize(group)
  return (
    MetadataUtils.getChildrenUnordered(metadata, path)
      .map((child) => MetadataUtils.findElementByElementPath(metadata, child.elementPath))
      .map((child) => getGroupChildState(child, groupHasExplicitSize))
      .find(isInvalidGroupState) ?? 'valid'
  )
}

export function getGroupChildStateFromJSXElement(
  jsxElement: JSXElement,
  groupHasExplicitSize: boolean,
): GroupState {
  return (
    maybeGroupChildNotPositionAbsolutely(jsxElement) ??
    maybeGroupChildHasPercentagePinsWithoutGroupSize(jsxElement, groupHasExplicitSize) ??
    maybeGroupChildHasMissingPins(jsxElement) ??
    'valid'
  )
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
    return 'unknown'
  }

  return getGroupChildStateFromJSXElement(jsxElement, groupHasExplicitSize)
}

export function getGroupChildStateWithGroupMetadata(
  elementMetadata: ElementInstanceMetadata | null,
  group: ElementInstanceMetadata,
): GroupState {
  const groupElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(group)
  if (groupElement == null) {
    return 'unknown'
  }

  return getGroupChildState(elementMetadata, checkGroupHasExplicitSize(groupElement))
}

export function groupJSXElement(children: JSXElementChildren): JSXElementWithoutUID {
  return jsxElementWithoutUID(
    'Group',
    jsxAttributesFromMap({
      style: jsExpressionValue(
        // we need to add position: absolute and top, left so that the TRUE_UP_GROUPS action can correct these values later
        { position: 'absolute', left: 0, top: 0 },
        emptyComments,
      ),
    }),
    children,
  )
}

export function groupJSXElementImportsToAdd(): Imports {
  return {
    'utopia-api': {
      importedAs: null,
      importedFromWithin: [importAlias('Group')],
      importedWithName: null,
    },
  }
}

export function maybeGroupHasPercentagePins(group: JSXElement | null): InvalidGroupState | null {
  if (group == null) {
    return null
  }
  return elementHasPercentagePins(group) ? 'group-has-percentage-pins' : null
}

export function maybeGroupChildHasMissingPins(
  jsxElement: JSXElement | null,
): InvalidGroupState | null {
  return jsxElement != null && !elementHasValidPins(jsxElement) ? 'child-has-missing-pins' : null
}

export function maybeGroupChildNotPositionAbsolutely(
  jsxElement: JSXElement | null,
): InvalidGroupState | null {
  if (jsxElement == null) {
    return null
  }
  const position = getLayoutProperty('position', right(jsxElement.props), styleStringInArray)
  return isLeft(position) || position.value !== 'absolute' ? 'child-not-position-absolute' : null
}

export function maybeGroupChildHasPercentagePinsWithoutGroupSize(
  jsxElement: JSXElement | null,
  groupHasExplicitSize: boolean,
): InvalidGroupState | null {
  if (jsxElement == null) {
    return 'unknown'
  }
  return !groupHasExplicitSize && elementHasPercentagePins(jsxElement)
    ? 'child-has-percentage-pins-without-group-size'
    : null
}

export function maybeGroupChildWithoutFixedSizeForFill(
  group: JSXElement | null,
): InvalidGroupState | null {
  return !checkGroupHasExplicitSize(group) ? 'child-has-percentage-pins-without-group-size' : null
}

export function groupErrorToastCommand(state: InvalidGroupState): ShowToastCommand {
  return showToastCommand(invalidGroupStateToString(state), 'ERROR', state)
}

export function groupErrorToastAction(state: InvalidGroupState): AddToast {
  return showToast(notice(invalidGroupStateToString(state), 'ERROR'))
}

export function maybeInvalidGroupStates(
  paths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  onGroup: (group: ElementPath) => InvalidGroupState | null,
  onChildren: (child: ElementPath) => InvalidGroupState | null,
): InvalidGroupState | null {
  function getInvalidStatesOrNull(
    makeTarget: (path: ElementPath) => ElementPath,
    getInvalidState: (path: ElementPath) => InvalidGroupState | null,
  ) {
    return mapDropNulls(
      getInvalidState,
      paths.filter((path) =>
        MetadataUtils.isGroupAgainstImports(
          MetadataUtils.findElementByElementPath(metadata, makeTarget(path)),
        ),
      ),
    )
  }

  const states = [
    ...getInvalidStatesOrNull((path) => path, onGroup),
    ...getInvalidStatesOrNull(EP.parentPath, onChildren),
  ]
  return states.length > 0 ? states[0] : null
}

export function groupStateFromJSXElement(
  element: JSXElement,
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
): GroupState | null {
  if (MetadataUtils.isGroupAgainstImports(MetadataUtils.findElementByElementPath(metadata, path))) {
    return getGroupStateFromJSXElement(element, path, metadata)
  } else if (
    MetadataUtils.isGroupAgainstImports(
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(path)),
    )
  ) {
    const group = MetadataUtils.getJSXElementFromMetadata(metadata, EP.parentPath(path))
    return getGroupChildStateFromJSXElement(element, checkGroupHasExplicitSize(group))
  } else {
    return null
  }
}
