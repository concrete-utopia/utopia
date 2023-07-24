import type { CSSProperties } from 'react'
import {
  MetadataUtils,
  getZIndexOrderedViewsWithoutDirectChildren,
} from '../../../../core/model/element-metadata-utils'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import { isLeft, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXAttributes,
  JSXElement,
  JSXElementLike,
} from '../../../../core/shared/element-template'
import {
  emptyComments,
  isJSXElement,
  isJSXElementLike,
  isJSXFragment,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxElement,
  jsxElementName,
  jsxFragment,
} from '../../../../core/shared/element-template'
import type {
  CanvasPoint,
  CanvasRectangle,
  LocalPoint,
  LocalRectangle,
  Size,
} from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  canvasRectangleToLocalRectangle,
  forceFiniteRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  sizeFromRectangle,
  zeroCanvasPoint,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import { forceNotNull, optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath, Imports } from '../../../../core/shared/project-file-types'
import { importAlias } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import type { Absolute } from '../../../../utils/utils'
import { absolute, back } from '../../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../../assets'
import { notice } from '../../../common/notice'
import type { AddToast, ApplyCommandsAction } from '../../../editor/action-types'
import { applyCommandsAction, showToast } from '../../../editor/actions/action-creators'
import type { AllElementProps, NavigatorEntry } from '../../../editor/store/editor-state'
import {
  childInsertionPath,
  commonInsertionPathFromArray,
} from '../../../editor/store/insertion-path'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import { cssPixelLength } from '../../../inspector/common/css-utils'
import {
  nukeAllAbsolutePositioningPropsCommands,
  nukeSizingPropsForAxisCommand,
  setElementTopLeft,
} from '../../../inspector/inspector-common'
import { EdgePositionBottomRight } from '../../canvas-types'
import { addElement } from '../../commands/add-element-command'
import type { CanvasCommand } from '../../commands/commands'
import { deleteElement } from '../../commands/delete-element-command'
import { queueGroupTrueUp } from '../../commands/queue-group-true-up-command'
import type { SetCssLengthProperty } from '../../commands/set-css-length-command'
import {
  setCssLengthProperty,
  setExplicitCssValue,
  setValueKeepingOriginalUnit,
} from '../../commands/set-css-length-command'
import { setProperty } from '../../commands/set-property-command'
import {
  getElementFragmentLikeType,
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  replaceNonDomElementWithFirstDomAncestorPath,
} from './fragment-like-helpers'
import type { AbsolutePin } from './resize-helpers'
import { ensureAtLeastTwoPinsForEdgePosition, isHorizontalPin } from './resize-helpers'

const GroupImport: Imports = {
  'utopia-api': {
    importedAs: null,
    importedFromWithin: [importAlias('Group')],
    importedWithName: null,
  },
}

export function isAbsolutePositionedFrame(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): boolean {
  return (
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(metadata, elementPath),
    ) &&
    MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath).length > 0 &&
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ).every((childPath) =>
      MetadataUtils.isPositionAbsolute(MetadataUtils.findElementByElementPath(metadata, childPath)),
    )
  )
}

// when removing the parent from between the children and the grandparent
function offsetChildrenByVectorCommands(
  childInstances: Array<ElementInstanceMetadata>,
  offset: CanvasPoint | LocalPoint,
) {
  return childInstances.flatMap((child) =>
    setElementTopLeft(child, {
      top: child.specialSizeMeasurements.offset.y + offset.y,
      left: child.specialSizeMeasurements.offset.x + offset.x,
    }),
  )
}

// when putting new container between the children and the grandparent
function offsetChildrenByDelta(
  childInstances: Array<ElementInstanceMetadata>,
  boundingFrame: CanvasRectangle,
) {
  return childInstances.flatMap((child) =>
    child.globalFrame != null && isFiniteRectangle(child.globalFrame)
      ? setElementTopLeft(child, {
          top: child.globalFrame.y - boundingFrame.y,
          left: child.globalFrame.x - boundingFrame.x,
        })
      : [],
  )
}

export function convertFragmentToSizelessDiv(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(parentPath),
      jsxElement(
        jsxElementName('div', []),
        uid,
        jsxAttributesFromMap({ 'data-uid': jsExpressionValue(uid, emptyComments) }),
        children,
      ),
      {
        indexPosition: absolute(
          MetadataUtils.getIndexInParent(metadata, elementPathTree, elementPath),
        ),
      },
    ),
  ]
}

export function convertFragmentToFrame(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
  convertIfStaticChildren:
    | 'do-not-convert-if-it-has-static-children'
    | 'convert-even-if-it-has-static-children',
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElementLike(element.element.value)) {
    return []
  }

  if (!isJSXFragment(element.element.value)) {
    // not a fragment, nothing to convert!
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  if (
    convertIfStaticChildren === 'do-not-convert-if-it-has-static-children' &&
    childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))
  ) {
    // if any children is not position: absolute, bail out from the conversion
    return []
  }

  const { children, uid } = element.element.value

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return []
  }

  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const left = childrenBoundingFrame.x - parentBounds.x
  const top = childrenBoundingFrame.y - parentBounds.y

  const fragmentIsCurrentlyAbsolute = element.specialSizeMeasurements.position === 'absolute'

  const absoluteTopLeftProps = fragmentIsCurrentlyAbsolute
    ? ({ position: 'absolute', top: top, left: left } as const)
    : ({ contain: 'layout' } as const)

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(parentPath),
      jsxElement(
        jsxElementName('div', []),
        uid,
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue(uid, emptyComments),
          style: jsExpressionValue(
            {
              ...absoluteTopLeftProps,
              width: childrenBoundingFrame.width,
              height: childrenBoundingFrame.height,
            },
            emptyComments,
          ),
        }),
        children,
      ),
      {
        indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
      },
    ),
    ...offsetChildrenByDelta(childInstances, childrenBoundingFrame),
  ]
}

export function convertFragmentToGroup(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): CanvasCommand[] {
  const parentPath = EP.parentPath(elementPath)
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElementLike(element.element.value)) {
    return []
  }

  if (!isJSXFragment(element.element.value)) {
    // not a fragment, nothing to convert!
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  if (childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))) {
    // if any children is not position: absolute, bail out from the conversion
    return []
  }

  const { children, uid } = element.element.value

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return []
  }

  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const left = childrenBoundingFrame.x - parentBounds.x
  const top = childrenBoundingFrame.y - parentBounds.y

  const fragmentIsCurrentlyAbsolute = element.specialSizeMeasurements.position === 'absolute'

  const absoluteTopLeftProps = fragmentIsCurrentlyAbsolute
    ? ({ position: 'absolute', top: top, left: left } as const)
    : ({ contain: 'layout' } as const)

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(parentPath),
      jsxElement(
        'Group',
        uid,
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue(uid, emptyComments),
          style: jsExpressionValue(
            {
              ...absoluteTopLeftProps,
              width: childrenBoundingFrame.width,
              height: childrenBoundingFrame.height,
            },
            emptyComments,
          ),
        }),
        children,
      ),
      {
        indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
        importsToAdd: GroupImport,
      },
    ),
    ...offsetChildrenByDelta(childInstances, childrenBoundingFrame),
  ]
}

export function convertGroupToFragment(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const { children, uid } = instance.element.value

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(parentPath), jsxFragment(uid, children, true), {
      indexPosition: absolute(
        MetadataUtils.getIndexInParent(metadata, elementPathTree, elementPath),
      ),
      importsToAdd: {
        react: {
          importedAs: 'React',
          importedFromWithin: [],
          importedWithName: null,
        },
      },
    }),
  ]
}

export function convertSizelessDivToFrameCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): CanvasCommand[] | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return null
  }

  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const left = childrenBoundingFrame.x - parentBounds.x
  const top = childrenBoundingFrame.y - parentBounds.y

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  return [
    setProperty('always', elementPath, PP.create('style', 'position'), 'absolute'),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'top'),
      setExplicitCssValue(cssPixelLength(top)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'left'),
      setExplicitCssValue(cssPixelLength(left)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'width'),
      setExplicitCssValue(cssPixelLength(childrenBoundingFrame.width)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    setCssLengthProperty(
      'always',
      elementPath,
      PP.create('style', 'height'),
      setExplicitCssValue(cssPixelLength(childrenBoundingFrame.height)),
      element?.specialSizeMeasurements.parentFlexDirection ?? null,
    ),
    ...offsetChildrenByDelta(childInstances, childrenBoundingFrame),
  ]
}

export function convertFrameToSizelessDivCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentOffset =
    MetadataUtils.findElementByElementPath(metadata, elementPath)?.specialSizeMeasurements.offset ??
    zeroCanvasPoint

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  return [
    ...nukeAllAbsolutePositioningPropsCommands(elementPath),
    nukeSizingPropsForAxisCommand('vertical', elementPath),
    nukeSizingPropsForAxisCommand('horizontal', elementPath),
    ...offsetChildrenByVectorCommands(childInstances, parentOffset),
  ]
}

export function convertFrameToFragmentCommands(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  // if any children is not position: absolute, bail out from the conversion
  if (childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))) {
    return []
  }

  const { children, uid } = instance.element.value

  const parentOffset =
    MetadataUtils.findElementByElementPath(metadata, elementPath)?.specialSizeMeasurements.offset ??
    zeroCanvasPoint

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(parentPath), jsxFragment(uid, children, true), {
      indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
      importsToAdd: {
        react: {
          importedAs: 'React',
          importedFromWithin: [],
          importedWithName: null,
        },
      },
    }),
    ...offsetChildrenByVectorCommands(childInstances, parentOffset),
  ]
}

export function convertFrameToGroup(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElement(instance.element.value)) {
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  if (childInstances.length === 0) {
    // if the Frame has no children, it cannot become a Group
    return []
  }

  // if any children is not position: absolute, bail out from the conversion
  if (childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))) {
    return []
  }

  const { children, uid, props } = instance.element.value
  const elementToAdd = jsxElement('Group', uid, props, children)

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(parentPath), elementToAdd, {
      indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
      importsToAdd: GroupImport,
    }),
    queueGroupTrueUp(childInstances[0].elementPath), // let the editor know that the children are positioned correctly and the Group needs to be shifted/resized
  ]
}

export function convertGroupToFrameCommands(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const parentPath = EP.parentPath(elementPath)
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElement(instance.element.value)) {
    return []
  }

  const childInstances = mapDropNulls(
    (path) => MetadataUtils.findElementByElementPath(metadata, path),
    replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      pathTrees,
      MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath),
    ),
  )

  // if any children is not position: absolute, bail out from the conversion
  if (childInstances.some((child) => MetadataUtils.elementParticipatesInAutoLayout(child))) {
    return []
  }

  const { children, uid, props } = instance.element.value

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(parentPath), jsxElement('div', uid, props, children), {
      indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
    }),
  ]
}

export function groupConversionCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): Array<CanvasCommand> | null {
  const fragmentLikeType = getElementFragmentLikeType(
    metadata,
    allElementProps,
    pathTrees,
    elementPath,
  )

  if (fragmentLikeType === 'fragment' || fragmentLikeType === 'conditional') {
    return null
  }

  if (fragmentLikeType === 'sizeless-div') {
    const convertCommands = convertSizelessDivToFrameCommands(
      metadata,
      allElementProps,
      pathTrees,
      elementPath,
    )
    if (convertCommands != null) {
      return convertCommands
    }
  }

  return null
}

export function createWrapInGroupActions(
  selectedViews: Array<ElementPath>,
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  elementPathTrees: ElementPathTrees,
  navigatorTargets: Array<NavigatorEntry>,
): ApplyCommandsAction | AddToast {
  // this arkane knowledge of the ancients came from WRAP_IN_ELEMENT
  const orderedActionTargets = getZIndexOrderedViewsWithoutDirectChildren(
    selectedViews,
    navigatorTargets,
  )

  // first, figure out the common ancestor
  const parentPath = commonInsertionPathFromArray(
    metadata,
    orderedActionTargets.map((actionTarget) => {
      return MetadataUtils.getReparentTargetOfTarget(metadata, actionTarget)
    }),
    'replace',
  )

  if (parentPath == null) {
    return showToast(notice('Wrap in Group failed: Could not find common parent.', 'ERROR'))
  }

  // if any target is a root element, refuse wrapping and show toast!
  const anyTargetIsARootElement = orderedActionTargets.some(EP.isRootElementOfInstance)
  if (anyTargetIsARootElement) {
    return showToast(notice("Root elements can't be wrapped into other elements", 'ERROR'))
  }

  const anyTargetIsNotJSXElement = orderedActionTargets.some(
    (e) => !isJSXElementLike(MetadataUtils.getJsxElementChildFromMetadata(metadata, e)),
  )
  if (anyTargetIsNotJSXElement) {
    return showToast(
      notice('Only simple JSX Elements can be wrapped into Groups for now 🙇', 'ERROR'),
    )
  }

  // TODO if any target doesn't honour the size or offset prop, refuse wrapping and show toast!

  const globalBoundingBoxOfAllElementsToBeWrapped: CanvasRectangle = forceNotNull(
    'boundingRectangleArray was somehow null!',
    boundingRectangleArray(
      orderedActionTargets.map((p) => MetadataUtils.getFrameOrZeroRectInCanvasCoords(p, metadata)),
    ),
  )

  const newLocalRectangleForGroup: LocalRectangle = forceNotNull(
    'groupLocalRect was somehow null!',
    MetadataUtils.getFrameRelativeToTargetContainingBlock(
      parentPath.intendedParentPath,
      metadata,
      globalBoundingBoxOfAllElementsToBeWrapped,
    ),
  )

  const childComponents: Array<{ element: JSXElementLike; metadata: ElementInstanceMetadata }> =
    orderedActionTargets.map((p) => {
      const foundMetadata = MetadataUtils.findElementByElementPath(metadata, p)
      const element = foundMetadata?.element
      if (
        foundMetadata == null ||
        element == null ||
        isLeft(element) ||
        !isJSXElementLike(element.value)
      ) {
        throw new Error(
          `Invariant violation: ElementInstanceMetadata.element found for ${EP.toString(
            p,
          )} was null or Left or not JSXElement`,
        )
      }
      return { element: element.value, metadata: foundMetadata }
    })

  // delete all reparented elements first to avoid UID clashes
  const deleteCommands = orderedActionTargets.map((e) => deleteElement('always', e))

  // TODO this is horrible and temporary at best. Instead of this, we should fix layoutSystemForChildren for Fragments in fillGlobalContentBoxFromAncestors
  const targetParentIsFlex = MetadataUtils.isFlexLayoutedContainer(
    MetadataUtils.findElementByElementPath(
      metadata,
      replaceNonDomElementWithFirstDomAncestorPath(
        metadata,
        allElementProps,
        elementPathTrees,
        parentPath.intendedParentPath,
      ),
    ),
  )

  // if we insert the group into a Flex parent, do not make it position: absolute and do not give it left, top pins
  const maybePositionAbsolute: CSSProperties = targetParentIsFlex
    ? { contain: 'layout' }
    : { position: 'absolute', left: newLocalRectangleForGroup.x, top: newLocalRectangleForGroup.y }

  // create a group with all elements as children
  const group = jsxElement(
    'Group',
    generateUidWithExistingComponents(projectContents),
    jsxAttributesFromMap({
      style: jsExpressionValue(
        // set group size here so we don't have to true it up
        {
          ...maybePositionAbsolute,
          width: newLocalRectangleForGroup.width,
          height: newLocalRectangleForGroup.height,
        },
        emptyComments,
      ),
    }),
    childComponents.map((c) => c.element),
  )

  // if any group child was a child of the group's target parent, let's use the child's original index for the insertion
  const anyChildIndexInTargetParent: Absolute | undefined = mapDropNulls((child) => {
    const childIsTheChildOfTargetParent = EP.isParentOf(
      parentPath.intendedParentPath,
      child.metadata.elementPath,
    )
    if (!childIsTheChildOfTargetParent) {
      return null
    }

    const indexInParent = MetadataUtils.getIndexInParent(
      metadata,
      elementPathTrees,
      child.metadata.elementPath,
    )
    if (indexInParent < 0) {
      return null
    }
    return absolute(indexInParent)
  }, childComponents).at(0)

  const indexPosition = anyChildIndexInTargetParent ?? back()

  // insert a group in the common ancestor
  const insertGroupCommand = addElement('always', parentPath, group, {
    importsToAdd: GroupImport,
    indexPosition: indexPosition,
  })

  const groupPath = EP.appendToPath(parentPath.intendedParentPath, group.uid) // TODO does this work if the parentPath is a ConditionalClauseInsertionPath?

  const retargetedChildComponentsForPinChanges: Array<{
    element: JSXElement
    metadata: ElementInstanceMetadata
    expectedElementPath: ElementPath
  }> = orderedActionTargets.flatMap((p) => {
    return replaceFragmentLikePathsWithTheirChildrenRecursive(
      metadata,
      allElementProps,
      elementPathTrees,
      [p],
    ).flatMap((maybeRetargetedPath) => {
      const expectedPath = forceNotNull(
        `invariant violation: no common path found between element and its descendants`,
        EP.replaceIfAncestor(maybeRetargetedPath, EP.parentPath(p), groupPath),
      )

      const foundMetadata = MetadataUtils.findElementByElementPath(metadata, maybeRetargetedPath)
      const element = foundMetadata?.element
      if (
        foundMetadata == null ||
        element == null ||
        isLeft(element) ||
        !isJSXElement(element.value)
      ) {
        throw new Error(
          `Invariant violation: ElementInstanceMetadata.element found for ${EP.toString(
            maybeRetargetedPath,
          )} was null or Left or not JSXElement`,
        )
      }
      return { element: element.value, metadata: foundMetadata, expectedElementPath: expectedPath }
    })
  })

  const pinChangeCommands = retargetedChildComponentsForPinChanges.flatMap((childComponent) => {
    const newChildPath = childComponent.expectedElementPath
    const childLocalRect: LocalRectangle = canvasRectangleToLocalRectangle(
      forceFiniteRectangle(childComponent.metadata.globalFrame),
      globalBoundingBoxOfAllElementsToBeWrapped,
    )
    return [
      // make the child `position: absolute`
      setProperty('always', newChildPath, PP.create('style', 'position'), 'absolute'),
      // set child pins to match their intended new local rectangle
      ...setElementPinsForLocalRectangleEnsureTwoPinsPerDimension(
        newChildPath,
        childComponent.element.props,
        childLocalRect,
        sizeFromRectangle(newLocalRectangleForGroup),
        null,
      ),
    ]
  })

  return applyCommandsAction([...deleteCommands, insertGroupCommand, ...pinChangeCommands])
}

function setElementPinsForLocalRectangleEnsureTwoPinsPerDimension(
  target: ElementPath,
  elementCurrentProps: JSXAttributes,
  localFrame: LocalRectangle,
  parentSize: Size,
  parentFlexDirection: FlexDirection | null,
): Array<CanvasCommand> {
  // ensure at least two pins per dimension
  const mustHavePins = ensureAtLeastTwoPinsForEdgePosition(
    right(elementCurrentProps),
    EdgePositionBottomRight,
  )

  function setPin(pin: AbsolutePin, value: number): SetCssLengthProperty {
    return setCssLengthProperty(
      'always',
      target,
      PP.create('style', pin),
      setValueKeepingOriginalUnit(
        value,
        isHorizontalPin(pin) ? parentSize.width : parentSize.height,
      ),
      parentFlexDirection,
      mustHavePins.includes(pin) ? 'create-if-not-existing' : 'do-not-create-if-doesnt-exist',
    )
  }

  // TODO retarget Fragments
  const result = [
    setPin('left', localFrame.x),
    setPin('top', localFrame.y),
    setPin('right', parentSize.width - (localFrame.x + localFrame.width)),
    setPin('bottom', parentSize.height - (localFrame.y + localFrame.height)),
    setPin('width', localFrame.width),
    setPin('height', localFrame.height),
  ]
  return result
}
