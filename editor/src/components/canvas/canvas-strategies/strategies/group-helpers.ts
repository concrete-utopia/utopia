import type { ProjectContentTreeRoot } from '../../../../components/assets'
import type { AllElementProps } from '../../../../components/editor/store/editor-state'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import {
  MetadataUtils,
  getSimpleAttributeAtPath,
} from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import type { Either } from '../../../../core/shared/either'
import { isLeft, isRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXElement,
  JSXElementChild,
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
import { isCSSNumber } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { MaxContent } from '../../../inspector/inspector-common'
import type { ShowToastCommand } from '../../commands/show-toast-command'
import { showToastCommand } from '../../commands/show-toast-command'
import { replaceNonDOMElementPathsWithTheirChildrenRecursive } from './fragment-like-helpers'
import type { AbsolutePin } from './resize-helpers'
import { horizontalPins, verticalPins } from './resize-helpers'

// Returns true if the element should be treated as a group,
// even if it's configuration (including its children) means that we cannot do any
// group manipulation operations.
export function treatElementAsGroupLike(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): boolean {
  return treatElementAsGroupLikeFromMetadata(MetadataUtils.findElementByElementPath(metadata, path))
}

export function treatElementAsGroupLikeFromMetadata(
  metadata: ElementInstanceMetadata | null,
): boolean {
  return MetadataUtils.isGroupAgainstImports(metadata)
}

// Determines if the element can be trued up as a group depending on how it has been configured.
export function allowGroupTrueUp(
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  path: ElementPath,
): boolean {
  const isGroupLike = treatElementAsGroupLike(metadata, path)
  if (isGroupLike) {
    const groupValidity = getGroupValidity(
      path,
      metadata,
      pathTrees,
      allElementProps,
      projectContents,
    )
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
  | 'child-has-percentage-pins'
  | 'child-has-missing-pins'
  | 'child-does-not-honour-props-position'
  | 'child-does-not-honour-props-size'
  | 'group-has-percentage-pins'
  | 'unknown'

export function groupValidityFromInvalidGroupState(groupState: InvalidGroupState): GroupValidity {
  switch (groupState) {
    case 'child-has-percentage-pins':
    case 'child-has-missing-pins':
    case 'group-has-percentage-pins':
      return 'warning'
    case 'child-not-position-absolute':
    case 'child-does-not-honour-props-position':
    case 'child-does-not-honour-props-size':
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
  return s != null && s !== 'valid'
}

export function invalidGroupStateToString(state: InvalidGroupState): string {
  switch (state) {
    // group state
    case 'group-has-percentage-pins':
      return `Groups shouldn't use % pins`

    // children state
    case 'child-not-position-absolute':
      return 'All children of group should have absolute position'
    case 'child-has-percentage-pins':
      return 'Children of a group should not have percentage pins'
    case 'child-has-missing-pins':
      return 'All children of group should have valid pins'
    case 'child-does-not-honour-props-position':
      return 'All children of group should honour position properties'
    case 'child-does-not-honour-props-size':
      return 'All children of group should honour size properties'

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

function getLayoutPropVerbatim(props: PropsOrJSXAttributes, pin: AbsolutePin): Either<string, any> {
  return getSimpleAttributeAtPath(props, stylePropPathMappingFn(pin, styleStringInArray))
}

function elementHasValidPins(jsxElement: JSXElement): boolean {
  function isMaxContentWidthOrHeight(pin: AbsolutePin): boolean {
    if (pin !== 'width' && pin !== 'height') {
      return false
    }

    // max-content (hug) is fine
    const verbatimProp = getLayoutPropVerbatim(right(jsxElement.props), pin)
    return isRight(verbatimProp) && verbatimProp.value === MaxContent
  }

  function containsPin(pin: AbsolutePin): boolean {
    const prop = getLayoutProperty(pin, right(jsxElement.props), styleStringInArray)
    const isNumericPin = isRight(prop) && prop.value != null

    return isNumericPin || isMaxContentWidthOrHeight(pin)
  }

  return (
    horizontalPins.filter(containsPin).length >= 2 && verticalPins.filter(containsPin).length >= 2
  )
}

export function getGroupStateFromJSXElement(
  jsxElement: JSXElement,
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
): GroupState {
  return (
    maybeGroupHasPercentagePins(jsxElement) ??
    maybeInvalidGroupChildren(
      jsxElement,
      path,
      metadata,
      pathTrees,
      allElementProps,
      projectContents,
    ) ??
    'valid'
  )
}

export function getGroupState(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
): GroupState {
  const group = MetadataUtils.getJSXElementFromMetadata(metadata, path)
  if (group == null) {
    return 'unknown'
  }
  return getGroupStateFromJSXElement(
    group,
    path,
    metadata,
    pathTrees,
    allElementProps,
    projectContents,
  )
}

function maybeInvalidGroupChildren(
  group: JSXElement,
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
): InvalidGroupState | 'valid' {
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
      const childGroupState = getGroupChildState(projectContents, childMetadata)
      if (isInvalidGroupState(childGroupState)) {
        return childGroupState
      }
    }
  }
  return 'valid'
}

export function getGroupChildStateFromJSXElement(jsxElement: JSXElement): GroupState {
  return (
    maybeGroupChildNotPositionAbsolutely(jsxElement) ??
    maybeGroupChildHasPercentagePinsWithoutGroupSize(jsxElement) ??
    maybeGroupChildHasMissingPins(jsxElement) ??
    'valid'
  )
}

export function getGroupValidity(
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
): GroupValidity {
  const groupState = getGroupState(path, metadata, pathTrees, allElementProps, projectContents)
  return groupValidityFromGroupState(groupState)
}

function getGroupChildState(
  projectContents: ProjectContentTreeRoot,
  elementMetadata: ElementInstanceMetadata | null,
): GroupState {
  if (elementMetadata == null) {
    return 'unknown'
  }

  const jsxElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(elementMetadata)
  if (jsxElement?.props == null) {
    return 'unknown'
  }

  if (!MetadataUtils.targetHonoursPropsPosition(projectContents, elementMetadata)) {
    return 'child-does-not-honour-props-position'
  }

  if (!MetadataUtils.targetHonoursPropsSize(projectContents, elementMetadata)) {
    return 'child-does-not-honour-props-size'
  }

  return getGroupChildStateFromJSXElement(jsxElement)
}

export function getGroupChildStateWithGroupMetadata(
  projectContents: ProjectContentTreeRoot,
  elementMetadata: ElementInstanceMetadata | null,
  group: ElementInstanceMetadata,
): GroupState {
  const groupElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(group)
  if (groupElement == null) {
    return 'unknown'
  }

  return getGroupChildState(projectContents, elementMetadata)
}

export function isMaybeGroupForWrapping(element: JSXElementChild, imports: Imports): boolean {
  const group = groupJSXElement([])
  const groupImports = groupJSXElementImportsToAdd()
  return (
    element.type === group.type &&
    element.name.baseVariable === group.name.baseVariable &&
    imports['utopia-api'] != null &&
    imports['utopia-api'].importedFromWithin[0].name ===
      groupImports['utopia-api'].importedFromWithin[0].name &&
    imports['utopia-api'].importedFromWithin[0].alias ===
      groupImports['utopia-api'].importedFromWithin[0].alias
  )
}

export function groupJSXElement(children: JSXElementChildren): JSXElementWithoutUID {
  return jsxElementWithoutUID(
    'Group',
    jsxAttributesFromMap({
      style: jsExpressionValue(
        // we need to add position: absolute and top, left so that the TRUE_UP_ELEMENTS action can correct these values later
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
): InvalidGroupState | null {
  if (jsxElement == null) {
    return 'unknown'
  }
  return elementHasPercentagePins(jsxElement) ? 'child-has-percentage-pins' : null
}

export function groupErrorToastCommand(state: InvalidGroupState): ShowToastCommand {
  return showToastCommand(invalidGroupStateToString(state), 'ERROR', state)
}

export function groupErrorToastAction(state: InvalidGroupState): AddToast {
  return showToast(notice(invalidGroupStateToString(state), 'ERROR'))
}

/**
 * This function runs over a list of ElementPaths and returns whether _any_
 * of the related elements is a group or a group child in an invalid configuration.
 * If none are found, null is returned instead.
 * @param paths The element paths to check.
 * @param metadata The metadata map.
 * @param checks Two functions that run on either groups or group children found in the the paths.
 * @returns The first invalid state found in the paths, or null otherwise.
 */
export function maybeInvalidGroupState(
  paths: ElementPath[],
  metadata: ElementInstanceMetadataMap,
  checks: {
    onGroup: (group: ElementPath) => InvalidGroupState | null
    onGroupChild: (child: ElementPath) => InvalidGroupState | null
  },
): InvalidGroupState | null {
  // This function performs the actual check on a filtered subset of the paths.
  function getInvalidStatesOrNull(
    type: 'group' | 'group-child',
    getInvalidState: (path: ElementPath) => InvalidGroupState | null,
  ) {
    // Calculate the subset of paths which are either groups or group children.
    // The distinction comes from the makeTarget argument which will effectively return either
    // the path itself (for groups) or the parent of the element (for group children).
    const targets = paths.filter((path) => {
      const targetPath = type === 'group-child' ? EP.parentPath(path) : path
      return treatElementAsGroupLike(metadata, targetPath)
    })
    return mapDropNulls(getInvalidState, targets)
  }

  const states = [
    // check for group invalid states
    ...getInvalidStatesOrNull('group', checks.onGroup),
    // check for group children invalid states
    ...getInvalidStatesOrNull('group-child', checks.onGroupChild),
  ]
  return states.length > 0 ? states[0] : null
}

export function groupStateFromJSXElement(
  element: JSXElement,
  path: ElementPath,
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  projectContents: ProjectContentTreeRoot,
): GroupState | null {
  if (treatElementAsGroupLike(metadata, path)) {
    // group
    return getGroupStateFromJSXElement(
      element,
      path,
      metadata,
      pathTrees,
      allElementProps,
      projectContents,
    )
  } else if (treatElementAsGroupLike(metadata, EP.parentPath(path))) {
    // group child
    return getGroupChildStateFromJSXElement(element)
  } else {
    // not a group
    return null
  }
}

export function isEmptyGroup(metadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
  return (
    treatElementAsGroupLike(metadata, path) &&
    MetadataUtils.getChildrenUnordered(metadata, path).length === 0
  )
}
