import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../../../core/shared/array-utils'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
} from '../../../../core/shared/element-template'
import {
  emptyComments,
  isJSXElement,
  isJSXElementLike,
  isJSXFragment,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxElement,
  jsxElementFromJSXElementWithoutUID,
  jsxElementName,
  jsxFragment,
} from '../../../../core/shared/element-template'
import type { CanvasPoint, LocalPoint, CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  zeroCanvasPoint,
  isFiniteRectangle,
  isInfinityRectangle,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath, Imports } from '../../../../core/shared/project-file-types'
import { importAlias } from '../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { cssPixelLength } from '../../../inspector/common/css-utils'
import {
  nukeAllAbsolutePositioningPropsCommands,
  nukeSizingPropsForAxisCommand,
  setElementTopLeft,
} from '../../../inspector/inspector-common'
import type { CanvasCommand } from '../../commands/commands'
import { setCssLengthProperty, setExplicitCssValue } from '../../commands/set-css-length-command'
import { setProperty } from '../../commands/set-property-command'
import {
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  getElementFragmentLikeType,
} from './fragment-like-helpers'
import * as PP from '../../../../core/shared/property-path'
import * as EP from '../../../../core/shared/element-path'
import { isLeft } from '../../../../core/shared/either'
import { deleteElement } from '../../commands/delete-element-command'
import { absolute } from '../../../../utils/utils'
import { addElement } from '../../commands/add-element-command'
import { childInsertionPath } from '../../../editor/store/insertion-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { queueGroupTrueUp } from '../../commands/queue-group-true-up-command'
import type { AddToast, WrapInElement } from '../../../editor/action-types'
import { showToast, wrapInElement } from '../../../editor/actions/action-creators'
import type { ProjectContentTreeRoot } from '../../../assets'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import { notice } from '../../../common/notice'
import { groupJSXElement, groupJSXElementImportsToAdd } from './group-helpers'

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

export function createWrapInGroupAction(
  selectedViews: Array<ElementPath>,
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
): WrapInElement | AddToast {
  const everySelectedViewPositionAbsolute = selectedViews.every((sv) =>
    MetadataUtils.isPositionAbsolute(MetadataUtils.findElementByElementPath(metadata, sv)),
  )
  if (!everySelectedViewPositionAbsolute) {
    return showToast(
      notice('Only `position: absolute` elements can be grouped for now. 🙇', 'ERROR'),
    )
  }
  return wrapInElement(selectedViews, {
    element: jsxElementFromJSXElementWithoutUID(
      groupJSXElement([]),
      generateUidWithExistingComponents(projectContents),
    ),
    importsToAdd: groupJSXElementImportsToAdd(),
  })
}
