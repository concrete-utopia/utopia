import type { ProjectContentTreeRoot } from '../../../../components/assets'
import {
  trueUpGroupElementChanged,
  type AllElementProps,
  trueUpChildrenOfGroupChanged,
} from '../../../../components/editor/store/editor-state'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { LayoutPinnedProp, StyleLayoutProp } from '../../../../core/layout/layout-helpers-new'
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
import {
  boundingRectangleArray,
  canvasRectangle,
  isFiniteRectangle,
  zeroRectangle,
} from '../../../../core/shared/math-utils'
import type { ElementPath, Imports } from '../../../../core/shared/project-file-types'
import { importAlias } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { assertNever } from '../../../../core/shared/utils'
import { styleStringInArray } from '../../../../utils/common-constants'
import { notice } from '../../../common/notice'
import type { AddToast } from '../../../editor/action-types'
import { showToast } from '../../../editor/actions/action-creators'
import { cssNumber, isCSSNumber } from '../../../inspector/common/css-utils'
import { stylePropPathMappingFn } from '../../../inspector/common/property-path-hooks'
import { MaxContent } from '../../../inspector/inspector-common'
import type { CanvasCommand } from '../../commands/commands'
import { deleteProperties } from '../../commands/delete-properties-command'
import { queueTrueUpElement } from '../../commands/queue-true-up-command'
import { setCssLengthProperty, setExplicitCssValue } from '../../commands/set-css-length-command'
import { setProperty } from '../../commands/set-property-command'
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
    getLayoutProperty('right', right(jsxElement.props), styleStringInArray),
    getLayoutProperty('bottom', right(jsxElement.props), styleStringInArray),
  ]
  return pins.some((pin) => {
    return isRight(pin) && isCSSNumber(pin.value) && pin.value.unit === '%'
  })
}

export type GroupChildPercentagePins = {
  width: number | null
  height: number | null
  left: number | null
  top: number | null
  bottom: number | null
  right: number | null
}

export function emptyGroupChildPercentagePins(): GroupChildPercentagePins {
  return {
    width: null,
    height: null,
    top: null,
    left: null,
    bottom: null,
    right: null,
  }
}

export function invalidPercentagePinsFromJSXElement(
  jsxElement: JSXElement | null,
): GroupChildPercentagePins {
  if (jsxElement?.props == null) {
    return emptyGroupChildPercentagePins()
  }
  function maybePercentValue(element: JSXElement, name: StyleLayoutProp): number | null {
    const pin = getLayoutProperty(name, right(element.props), styleStringInArray)
    return isRight(pin) && isCSSNumber(pin.value) && pin.value.unit === '%' ? pin.value.value : null
  }
  return {
    width: maybePercentValue(jsxElement, 'width'),
    height: maybePercentValue(jsxElement, 'height'),
    left: maybePercentValue(jsxElement, 'left'),
    top: maybePercentValue(jsxElement, 'top'),
    bottom: maybePercentValue(jsxElement, 'bottom'),
    right: maybePercentValue(jsxElement, 'right'),
  }
}

function getLayoutPropVerbatim(props: PropsOrJSXAttributes, pin: AbsolutePin): Either<string, any> {
  return getSimpleAttributeAtPath(props, stylePropPathMappingFn(pin, styleStringInArray))
}

function getNumericPin(jsxElement: JSXElement, pin: AbsolutePin) {
  const prop = getLayoutProperty(pin, right(jsxElement.props), styleStringInArray)
  const isNumericPin = isRight(prop) && prop.value != null
  if (!isNumericPin) {
    return null
  }
  return prop.value
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
    return getNumericPin(jsxElement, pin) != null || isMaxContentWidthOrHeight(pin)
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
    maybeInvalidGroupChildren(path, metadata, pathTrees, allElementProps, projectContents) ??
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
    maybeGroupChildHasPercentagePins(jsxElement) ??
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

export function getGroupChildState(
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

  if (
    MetadataUtils.targetHonoursPropsPosition(projectContents, elementMetadata) === 'does-not-honour'
  ) {
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

export function maybeGroupChildHasPercentagePins(
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

function fixLengthCommand(path: ElementPath, prop: LayoutPinnedProp, size: number): CanvasCommand {
  return setCssLengthProperty(
    'always',
    path,
    PP.create('style', prop),
    setExplicitCssValue(cssNumber(size, 'px')),
    null,
    'create-if-not-existing',
  )
}

function fixGroupCommands(jsxMetadata: ElementInstanceMetadataMap, path: ElementPath) {
  let commands: CanvasCommand[] = []

  const metadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
  if (
    metadata == null ||
    metadata.globalFrame == null ||
    !isFiniteRectangle(metadata.globalFrame)
  ) {
    return []
  }
  const frame = metadata.globalFrame

  const children = MetadataUtils.getChildrenUnordered(jsxMetadata, path)
  const childFrames = children.map((c) =>
    c.globalFrame != null && isFiniteRectangle(c.globalFrame)
      ? c.globalFrame
      : canvasRectangle(zeroRectangle),
  )
  const childrenBounds = boundingRectangleArray(childFrames) ?? frame

  // must have non-percentage pins
  const jsxElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(metadata)
  const invalidPins = invalidPercentagePinsFromJSXElement(jsxElement)
  if (invalidPins.width != null) {
    commands.push(fixLengthCommand(path, 'width', childrenBounds.width))
  }
  if (invalidPins.height != null) {
    commands.push(fixLengthCommand(path, 'height', childrenBounds.height))
  }

  const parentMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(path))
  const maybeParentFrame =
    parentMetadata?.globalFrame != null && isFiniteRectangle(parentMetadata.globalFrame)
      ? parentMetadata.globalFrame
      : null

  let left = jsxElement != null ? getNumericPin(jsxElement, 'left')?.value : null
  if (invalidPins.left != null) {
    left =
      maybeParentFrame != null
        ? maybeParentFrame.width * (invalidPins.left / 100)
        : childrenBounds.x
    commands.push(fixLengthCommand(path, 'left', left))
  }
  let top = jsxElement != null ? getNumericPin(jsxElement, 'top')?.value : null
  if (invalidPins.top != null) {
    top =
      maybeParentFrame != null
        ? maybeParentFrame.height * (invalidPins.top / 100)
        : childrenBounds.y
    commands.push(fixLengthCommand(path, 'top', top))
  }
  if (invalidPins.bottom != null) {
    const newTop =
      top != null
        ? top
        : maybeParentFrame != null
        ? (maybeParentFrame.height * (100 - invalidPins.bottom)) / 100.0 - childrenBounds.height
        : childrenBounds.y
    commands.push(
      fixLengthCommand(path, 'top', newTop),
      deleteProperties('always', path, [PP.create('style', 'bottom')]),
    )
  }
  if (invalidPins.right != null) {
    const newLeft =
      left != null
        ? left
        : maybeParentFrame != null
        ? (maybeParentFrame.width * (100 - invalidPins.right)) / 100.0 - childrenBounds.width
        : childrenBounds.x
    commands.push(
      fixLengthCommand(path, 'left', newLeft),
      deleteProperties('always', path, [PP.create('style', 'right')]),
    )
  }

  // apply children fixes too
  for (const child of children) {
    commands.push(...fixGroupChildCommands(jsxMetadata, child.elementPath))
  }

  return commands
}

function fixGroupChildCommands(jsxMetadata: ElementInstanceMetadataMap, path: ElementPath) {
  let commands: CanvasCommand[] = []

  const metadata = MetadataUtils.findElementByElementPath(jsxMetadata, path)
  if (
    metadata == null ||
    metadata.globalFrame == null ||
    !isFiniteRectangle(metadata.globalFrame)
  ) {
    return []
  }
  const frame = metadata.globalFrame

  const jsxElement = MetadataUtils.getJSXElementFromElementInstanceMetadata(metadata)
  if (jsxElement == null) {
    return []
  }

  const parentMetadata = MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(path))
  if (
    parentMetadata == null ||
    parentMetadata.globalFrame == null ||
    !isFiniteRectangle(parentMetadata.globalFrame)
  ) {
    return []
  }
  const parentFrame = parentMetadata.globalFrame

  // must have non-percent pins
  const invalidPins = invalidPercentagePinsFromJSXElement(jsxElement)
  if (invalidPins.width != null) {
    commands.push(fixLengthCommand(path, 'width', frame.width))
  }
  if (invalidPins.height != null) {
    commands.push(fixLengthCommand(path, 'height', frame.height))
  }
  let left = getNumericPin(jsxElement, 'left')?.value
  if (invalidPins.left != null) {
    left = parentFrame.width * (invalidPins.left / 100)
    commands.push(fixLengthCommand(path, 'left', left))
  }
  let top = getNumericPin(jsxElement, 'top')?.value
  if (invalidPins.top != null) {
    top = parentFrame.height * (invalidPins.top / 100)
    commands.push(fixLengthCommand(path, 'top', top))
  }
  if (invalidPins.bottom != null) {
    const newTop =
      top != null ? top : (parentFrame.height * (100 - invalidPins.bottom)) / 100.0 - frame.height
    commands.push(
      fixLengthCommand(path, 'top', newTop),
      deleteProperties('always', path, [PP.create('style', 'bottom')]),
    )
  }
  if (invalidPins.right != null) {
    const newLeft =
      left != null ? left : (parentFrame.width * (100 - invalidPins.right)) / 100.0 - frame.width
    commands.push(
      fixLengthCommand(path, 'left', newLeft),
      deleteProperties('always', path, [PP.create('style', 'right')]),
    )
  }

  // must have absolute position
  if (maybeGroupChildNotPositionAbsolutely(jsxElement) != null) {
    commands.push(setProperty('always', path, PP.create('style', 'position'), 'absolute'))
  }

  // must have valid pins
  if (maybeGroupChildHasMissingPins(jsxElement) != null) {
    commands.push(
      fixLengthCommand(path, 'width', getNumericPin(jsxElement, 'width')?.value ?? frame.width),
      fixLengthCommand(path, 'height', getNumericPin(jsxElement, 'height')?.value ?? frame.height),
      fixLengthCommand(path, 'left', getNumericPin(jsxElement, 'left')?.value ?? 0),
      fixLengthCommand(path, 'top', getNumericPin(jsxElement, 'top')?.value ?? 0),
    )
  }

  if (commands.length > 0) {
    commands.push(queueTrueUpElement([trueUpGroupElementChanged(path)]))
  }

  return commands
}

export type GroupProblem = {
  target: 'group' | 'group-child'
  path: ElementPath
  state: InvalidGroupState
}

export function getFixGroupProblemsCommands(
  jsxMetadata: ElementInstanceMetadataMap,
  problems: GroupProblem[],
): CanvasCommand[] {
  let commands: CanvasCommand[] = []
  for (const problem of problems) {
    switch (problem.target) {
      case 'group':
        commands.push(...fixGroupCommands(jsxMetadata, problem.path))
        break
      case 'group-child':
        commands.push(...fixGroupChildCommands(jsxMetadata, problem.path))
        break
      default:
        assertNever(problem.target)
    }
  }
  return commands
}
