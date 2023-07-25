import type { AllElementProps } from 'src/components/editor/store/editor-state'
import type { ElementPathTrees } from 'src/core/shared/element-path-tree'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { StyleLayoutProp } from '../../../../core/layout/layout-helpers-new'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { isRight, right } from '../../../../core/shared/either'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXElement,
  JSXElementChildren,
  JSXElementWithoutUID,
} from '../../../../core/shared/element-template'
import {
  jsxAttributesFromMap,
  jsxElementWithoutUID,
  emptyComments,
  jsExpressionValue,
} from '../../../../core/shared/element-template'
import type { ElementPath, Imports } from '../../../../core/shared/project-file-types'
import { importAlias } from '../../../../core/shared/project-file-types'
import { assertNever } from '../../../../core/shared/utils'
import { styleStringInArray } from '../../../../utils/common-constants'
import { isCSSNumber } from '../../../inspector/common/css-utils'
import { replaceNonDOMElementPathsWithTheirChildrenRecursive } from './fragment-like-helpers'

// Returns true if the element should be treated as a group,
// even if it's configuration (including its children) means that we cannot do any
// group manipulation operations.
export function treatElementAsGroupLike(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): boolean {
  return MetadataUtils.isGroupAgainstImports(MetadataUtils.findElementByElementPath(metadata, path))
}

// Determines if the element can be trued up as a group depending on how it has been configured.
export function allowGroupTrueUp(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  path: ElementPath,
): boolean {
  const isGroupLike = treatElementAsGroupLike(metadata, path)
  if (isGroupLike) {
    const groupValidity = getGroupValidity(path, metadata, pathTrees, allElementProps)
    switch (groupValidity) {
      case 'valid':
      case 'warning':
        return true
      case 'error':
        return false
      default:
        assertNever(groupValidity)
    }
  } else {
    return false
  }
}

export type GroupState = 'valid' | InvalidGroupState

export type GroupValidity = 'valid' | 'warning' | 'error'

export type InvalidGroupState =
  | 'child-not-position-absolute'
  | 'child-has-percentage-pins-without-group-size'
  | 'child-has-missing-pins'
  | 'group-has-percentage-pins'
  | 'unknown'

export function groupValidityFromInvalidGroupState(groupState: InvalidGroupState): GroupValidity {
  switch (groupState) {
    case 'child-has-percentage-pins-without-group-size':
    case 'child-has-missing-pins':
    case 'group-has-percentage-pins':
      return 'warning'
    case 'child-not-position-absolute':
    case 'unknown':
      return 'error'
    default:
      assertNever(groupState)
  }
}

export function groupValidityFromGroupState(groupState: GroupState): GroupValidity {
  switch (groupState) {
    case 'valid':
      return 'valid'
    default:
      return groupValidityFromInvalidGroupState(groupState)
  }
}

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

function elementHasValidPins(jsxElement: JSXElement): boolean {
  function isValidPin(name: StyleLayoutProp): boolean {
    const pin = getLayoutProperty(name, right(jsxElement.props), styleStringInArray)
    return isRight(pin) && isCSSNumber(pin.value)
  }
  const leftPin = isValidPin('left')
  const rightPin = isValidPin('right')
  const topPin = isValidPin('top')
  const bottomPin = isValidPin('bottom')

  if (leftPin || rightPin) {
    return topPin || bottomPin
  } else {
    return !(topPin || bottomPin)
  }
}

export function getGroupState(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
): GroupState {
  const group = MetadataUtils.getJSXElementFromMetadata(metadata, path)

  if (group == null) {
    return 'unknown'
  } else if (elementHasPercentagePins(group)) {
    return 'group-has-percentage-pins'
  } else {
    const groupHasExplicitSize = checkGroupHasExplicitSize(group)
    const childPaths = MetadataUtils.getChildrenUnordered(metadata, path).map(
      (child) => child.elementPath,
    )
    const flattenedChildPaths = replaceNonDOMElementPathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      childPaths,
    )
    for (const childPath of flattenedChildPaths) {
      const childMetadata = MetadataUtils.findElementByElementPath(metadata, childPath)
      if (childMetadata != null) {
        const childGroupState = getGroupChildState(childMetadata, groupHasExplicitSize)
        if (isInvalidGroupState(childGroupState)) {
          return childGroupState
        }
      }
    }
    return 'valid'
  }
}

export function getGroupValidity(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
): GroupValidity {
  const groupState = getGroupState(path, metadata, pathTrees, allElementProps)
  return groupValidityFromGroupState(groupState)
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
  } else if (!MetadataUtils.isPositionAbsolute(elementMetadata)) {
    return 'child-not-position-absolute'
  } else if (!elementHasValidPins(jsxElement)) {
    return 'child-has-missing-pins'
  } else if (!groupHasExplicitSize && elementHasPercentagePins(jsxElement)) {
    return 'child-has-percentage-pins-without-group-size'
  } else {
    return 'valid'
  }
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
