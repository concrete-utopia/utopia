import React from 'react'
import type { CSSProperties } from 'react'
import { getLayoutProperty } from '../../../../core/layout/getLayoutProperty'
import type { StyleLayoutProp } from '../../../../core/layout/layout-helpers-new'
import type { PropsOrJSXAttributes } from '../../../../core/model/element-metadata-utils'
import {
  MetadataUtils,
  getZIndexOrderedViewsWithoutDirectChildren,
} from '../../../../core/model/element-metadata-utils'
import {
  removeBackgroundProperties,
  removeMarginProperties,
  removePaddingProperties,
} from '../../../../core/model/margin-and-padding'
import { arrayAccumulate, mapDropNulls } from '../../../../core/shared/array-utils'
import { defaultEither, isLeft, isRight, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import type {
  ElementInstanceMetadata,
  ElementInstanceMetadataMap,
  JSXAttributes,
  JSXConditionalExpression,
  JSXElement,
  JSXElementChild,
  JSXElementChildren,
  JSXElementLike,
  JSXFragment,
  SpecialSizeMeasurements,
} from '../../../../core/shared/element-template'
import {
  emptyComments,
  isJSXConditionalExpression,
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
  canvasVector,
  forceFiniteRectangle,
  isFiniteRectangle,
  isInfinityRectangle,
  localRectangle,
  sizeFromRectangle,
  zeroCanvasPoint,
  zeroCanvasRect,
} from '../../../../core/shared/math-utils'
import {
  fromField,
  fromTypeGuard,
  traverseArray,
} from '../../../../core/shared/optics/optic-creators'
import { modify } from '../../../../core/shared/optics/optic-utilities'
import { forceNotNull, optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath, Imports } from '../../../../core/shared/project-file-types'
import { importAlias } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import { assertNever } from '../../../../core/shared/utils'
import { styleStringInArray } from '../../../../utils/common-constants'
import type { Absolute } from '../../../../utils/utils'
import { absolute, back } from '../../../../utils/utils'
import type { ProjectContentTreeRoot } from '../../../assets'
import { notice } from '../../../common/notice'
import type { AddToast, ApplyCommandsAction } from '../../../editor/action-types'
import { applyCommandsAction, showToast } from '../../../editor/actions/action-creators'
import type { AllElementProps, NavigatorEntry } from '../../../editor/store/editor-state'
import {
  trueUpChildrenOfGroupChanged,
  trueUpGroupElementChanged,
} from '../../../editor/store/editor-state'
import {
  childInsertionPath,
  commonInsertionPathFromArray,
  replaceWithSingleElement,
} from '../../../editor/store/insertion-path'
import type { FlexDirection } from '../../../inspector/common/css-utils'
import { cssPixelLength, isCSSNumber } from '../../../inspector/common/css-utils'
import {
  flexContainerProps,
  getConvertIndividualElementToAbsoluteCommandsFromMetadata,
  isHugFromStyleAttribute,
  nukeAllAbsolutePositioningPropsCommands,
  nukeSizingPropsForAxisCommand,
  prunePropsCommands,
  setElementTopLeft,
} from '../../../inspector/inspector-common'
import { EdgePositionBottomRight } from '../../canvas-types'
import { addElement } from '../../commands/add-element-command'
import { runCanvasCommand, type CanvasCommand } from '../../commands/commands'
import { deleteElement } from '../../commands/delete-element-command'
import { queueTrueUpElement } from '../../commands/queue-true-up-command'
import type { SetCssLengthProperty } from '../../commands/set-css-length-command'
import {
  setCssLengthProperty,
  setExplicitCssValue,
  setValueKeepingOriginalUnit,
} from '../../commands/set-css-length-command'
import { setProperty } from '../../commands/set-property-command'
import { updateSelectedViews } from '../../commands/update-selected-views-command'
import {
  getElementFragmentLikeType,
  replaceFragmentLikePathsWithTheirChildrenRecursive,
  replaceNonDomElementWithFirstDomAncestorPath,
} from './fragment-like-helpers'
import { isEmptyGroup } from './group-helpers'
import type { AbsolutePin } from './resize-helpers'
import { ensureAtLeastTwoPinsForEdgePosition, isHorizontalPin } from './resize-helpers'
import { getConditionalActiveCase } from '../../../../core/model/conditionals'
import { showToastCommand } from '../../commands/show-toast-command'
import { unsetJSXValueAtPath } from '../../../../core/shared/jsx-attributes'
import type { Optic } from '../../../../core/shared/optics/optics'
import type { EditorContract } from './contracts/contract-helpers'
import { useRefEditorState } from '../../../editor/store/store-hook'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { generateUID } from '../../../../core/shared/uid-utils'

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

type JSXElementConversion = JSXElement

type JSXElementLikeConversion = {
  element: JSXElementLike
  childInstances: ElementInstanceMetadata[]
}

export type JSXFragmentConversion = {
  element: JSXFragment
  childInstances: ElementInstanceMetadata[]
  childrenBoundingFrame: CanvasRectangle
  specialSizeMeasurements: SpecialSizeMeasurements
}

export function getInstanceForFragmentToFrameConversion(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
  convertIfStaticChildren:
    | 'do-not-convert-if-it-has-static-children'
    | 'convert-even-if-it-has-static-children',
): JSXFragmentConversion | ConversionForbidden {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElementLike(element.element.value)) {
    return conversionForbidden('Fragment is not a valid element')
  }

  if (!isJSXFragment(element.element.value)) {
    // not a fragment, nothing to convert!
    return conversionForbidden('Element is not a Fragment')
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
    return conversionForbidden('Fragment children must be positioned absolutely')
  }

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return conversionForbidden('Fragment has invalid children bounds')
  }

  return {
    element: element.element.value,
    specialSizeMeasurements: element.specialSizeMeasurements,
    childInstances: childInstances,
    childrenBoundingFrame: childrenBoundingFrame,
  }
}

export function actuallyConvertFramentToFrame(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  instance: JSXFragmentConversion,
  elementPath: ElementPath,
): CanvasCommand[] {
  const { children, uid } = instance.element

  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const left = instance.childrenBoundingFrame.x - parentBounds.x
  const top = instance.childrenBoundingFrame.y - parentBounds.y

  const fragmentIsCurrentlyAbsolute = instance.specialSizeMeasurements.position === 'absolute'

  const absoluteTopLeftProps = fragmentIsCurrentlyAbsolute
    ? ({ position: 'absolute', top: top, left: left } as const)
    : ({ contain: 'layout' } as const)

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(EP.parentPath(elementPath)),
      jsxElement(
        jsxElementName('div', []),
        uid,
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue(uid, emptyComments),
          style: jsExpressionValue(
            {
              ...absoluteTopLeftProps,
              width: instance.childrenBoundingFrame.width,
              height: instance.childrenBoundingFrame.height,
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
    ...offsetChildrenByDelta(instance.childInstances, instance.childrenBoundingFrame),
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
  const instance = getInstanceForFragmentToFrameConversion(
    metadata,
    pathTrees,
    allElementProps,
    elementPath,
    convertIfStaticChildren,
  )
  if (isConversionForbidden(instance)) {
    return []
  }

  return actuallyConvertFramentToFrame(metadata, pathTrees, instance, elementPath)
}

export function getInstanceForFragmentToGroupConversion(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): JSXFragmentConversion | ConversionForbidden {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null || isLeft(element.element) || !isJSXElementLike(element.element.value)) {
    return conversionForbidden('Fragment is not a valid element')
  }

  if (!isJSXFragment(element.element.value)) {
    // not a fragment, nothing to convert!
    return conversionForbidden('Element is not a Fragment')
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
    return conversionForbidden('Fragment children must be positioned absolutely')
  }

  const childrenBoundingFrame = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (childrenBoundingFrame == null || isInfinityRectangle(childrenBoundingFrame)) {
    return conversionForbidden('Fragment has invalid children bounds')
  }

  return {
    element: element.element.value,
    childrenBoundingFrame: childrenBoundingFrame,
    specialSizeMeasurements: element.specialSizeMeasurements,
    childInstances: childInstances,
  }
}

export function convertFragmentToGroup(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): CanvasCommand[] {
  const instance = getInstanceForFragmentToGroupConversion(
    metadata,
    pathTrees,
    allElementProps,
    elementPath,
  )
  if (isConversionForbidden(instance)) {
    return []
  }
  const parentBounds =
    optionalMap(
      MetadataUtils.getGlobalContentBoxForChildren,
      MetadataUtils.findElementByElementPath(metadata, EP.parentPath(elementPath)),
    ) ?? zeroCanvasRect

  const { children, uid } = instance.element

  // Remove the margins from the children.
  const toPropsOptic = traverseArray<JSXElementChild>()
    .compose(fromTypeGuard(isJSXElement))
    .compose(fromField('props'))
  const childrenWithoutMargins = modify(toPropsOptic, removeMarginProperties, children)

  const left = instance.childrenBoundingFrame.x - parentBounds.x
  const top = instance.childrenBoundingFrame.y - parentBounds.y

  const fragmentIsCurrentlyAbsolute = instance.specialSizeMeasurements.position === 'absolute'

  const absoluteTopLeftProps = fragmentIsCurrentlyAbsolute
    ? ({ position: 'absolute', top: top, left: left } as const)
    : ({ contain: 'layout' } as const)

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(EP.parentPath(elementPath)),
      jsxElement(
        'Group',
        uid,
        jsxAttributesFromMap({
          'data-uid': jsExpressionValue(uid, emptyComments),
          style: jsExpressionValue(
            {
              ...absoluteTopLeftProps,
              width: instance.childrenBoundingFrame.width,
              height: instance.childrenBoundingFrame.height,
            },
            emptyComments,
          ),
        }),
        childrenWithoutMargins,
      ),
      {
        indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
        importsToAdd: GroupImport,
      },
    ),
    ...offsetChildrenByDelta(instance.childInstances, instance.childrenBoundingFrame),
  ]
}

const childrenPropsOptic: Optic<JSXElementChildren, JSXAttributes> =
  traverseArray<JSXElementChild>().compose(fromTypeGuard(isJSXElement)).compose(fromField('props'))

function removeDataConstraintsFromChildren(children: JSXElementChildren): JSXElementChildren {
  // Remove the `data-constraints` property from each of the children passed in, if possible.
  return modify(
    childrenPropsOptic,
    (attributes) => {
      const updatedChildrenProps = unsetJSXValueAtPath(attributes, PP.create('data-constraints'))
      return defaultEither(attributes, updatedChildrenProps)
    },
    children,
  )
}

export function useConvertWrapperToFrame() {
  const dispatch = useDispatch()
  const editorStateRef = useRefEditorState((store) => store)
  return React.useCallback(() => {
    const { jsxMetadata, allElementProps, elementPathTree, selectedViews } =
      editorStateRef.current.editor
    dispatch([
      applyCommandsAction(
        selectedViews.flatMap((sv) =>
          convertWrapperToFrameCommands(jsxMetadata, allElementProps, elementPathTree, sv),
        ),
      ),
    ])
  }, [dispatch, editorStateRef])
}

function convertWrapperToFrameCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): CanvasCommand[] {
  return convertSizelessDivToFrameCommands(metadata, allElementProps, pathTrees, elementPath) ?? []
}

export function convertSizelessDivToFrameCommands(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): CanvasCommand[] | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)

  const childrenBoundingFrame = MetadataUtils.getBoundingRectangleOfChildren(
    metadata,
    pathTrees,
    elementPath,
  )
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

export function getInstanceForFrameToFragmentConversion(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): JSXElementLikeConversion | ConversionForbidden {
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElementLike(instance.element.value)) {
    return conversionForbidden('Frame is not a valid element')
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
    return conversionForbidden('Frame children must be positioned absolutely')
  }

  return { element: instance.element.value, childInstances: childInstances }
}

export function convertGroupOrFrameToFragmentCommands(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const instance = getInstanceForFrameToFragmentConversion(
    metadata,
    pathTrees,
    allElementProps,
    elementPath,
  )
  if (isConversionForbidden(instance)) {
    return []
  }

  const { children, uid } = instance.element
  const updatedChildren = removeDataConstraintsFromChildren(children)

  const parentOffset =
    MetadataUtils.findElementByElementPath(metadata, elementPath)?.specialSizeMeasurements.offset ??
    zeroCanvasPoint

  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(EP.parentPath(elementPath)),
      jsxFragment(uid, updatedChildren, true),
      {
        indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
        importsToAdd: {
          react: {
            importedAs: 'React',
            importedFromWithin: [],
            importedWithName: null,
          },
        },
      },
    ),
    ...offsetChildrenByVectorCommands(instance.childInstances, parentOffset),
  ]
}

type ConversionForbidden = {
  type: 'CONVERSION_FORBIDDEN'
  reason: string
}

function conversionForbidden(reason: string): ConversionForbidden {
  return {
    type: 'CONVERSION_FORBIDDEN',
    reason: reason,
  }
}

export function isConversionForbidden(c: unknown): c is ConversionForbidden {
  return (c as ConversionForbidden).type === 'CONVERSION_FORBIDDEN'
}

export function getInstanceForFrameToGroupConversion(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): JSXElementConversion | ConversionForbidden {
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElement(instance.element.value)) {
    return conversionForbidden('Frame is not a valid element')
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
    return conversionForbidden('Frame has no children')
  }

  return instance.element.value
}

export function convertFrameToGroup(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const instance = getInstanceForFrameToGroupConversion(
    metadata,
    pathTrees,
    allElementProps,
    elementPath,
  )
  if (isConversionForbidden(instance)) {
    return []
  }

  const { children, uid, props } = instance
  // Remove the padding and background properties from the parent.
  const propsWithoutPadding = removeBackgroundProperties(removePaddingProperties(props))
  // Remove the margin properties for the children of the newly created group.
  const toPropsOptic = traverseArray<JSXElementChild>()
    .compose(fromTypeGuard(isJSXElement))
    .compose(fromField('props'))
  const childrenWithoutMargins = modify(toPropsOptic, removeMarginProperties, children)
  const elementToAdd = jsxElement('Group', uid, propsWithoutPadding, childrenWithoutMargins)

  const childrenPaths = MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, elementPath)

  return [
    deleteElement('always', elementPath),
    addElement('always', childInsertionPath(EP.parentPath(elementPath)), elementToAdd, {
      indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
      importsToAdd: GroupImport,
    }),
    ...childrenPaths.flatMap((c) =>
      getConvertIndividualElementToAbsoluteCommandsFromMetadata(c, metadata, pathTrees),
    ),
    ...prunePropsCommands(flexContainerProps, elementPath), // flex-related stuff is pruned
    queueTrueUpElement([trueUpChildrenOfGroupChanged(elementPath)]),
    showToastCommand(
      'Converted to group and removed styling',
      'INFO',
      'convert-frame-to-group-toast',
    ),
  ]
}

export function getInstanceForGroupToFrameConversion(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): JSXElementConversion | ConversionForbidden {
  const instance = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (instance == null || isLeft(instance.element) || !isJSXElement(instance.element.value)) {
    return conversionForbidden('Group is not a valid element')
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
    return conversionForbidden('Group children must be positioned absolutely')
  }

  return instance.element.value
}

export function convertGroupToFrameCommands(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  elementPath: ElementPath,
): Array<CanvasCommand> {
  const instance = getInstanceForGroupToFrameConversion(
    metadata,
    pathTrees,
    allElementProps,
    elementPath,
  )
  if (isConversionForbidden(instance)) {
    return []
  }
  const { children, uid, props } = instance
  const updatedChildren = removeDataConstraintsFromChildren(children)
  return [
    deleteElement('always', elementPath),
    addElement(
      'always',
      childInsertionPath(EP.parentPath(elementPath)),
      jsxElement('div', uid, props, updatedChildren),
      {
        indexPosition: absolute(MetadataUtils.getIndexInParent(metadata, pathTrees, elementPath)),
      },
    ),
  ]
}

export type GroupChildElement = JSXElementLike | JSXConditionalExpression

export function elementCanBeAGroupChild(
  element: JSXElementChild | null,
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): element is GroupChildElement {
  if (element == null) {
    return false
  }
  if (isJSXElementLike(element)) {
    return true
  }
  if (isJSXConditionalExpression(element)) {
    const activeBranch = getConditionalActiveCase(elementPath, element, jsxMetadata)
    const branch = activeBranch === 'true-case' ? element.whenTrue : element.whenFalse
    return elementCanBeAGroupChild(branch, EP.appendToPath(elementPath, branch.uid), jsxMetadata)
  }
  return false
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
    replaceWithSingleElement(),
  )

  if (parentPath == null) {
    return showToast(notice('Wrap in Group failed: Could not find common parent.', 'ERROR'))
  }

  // if any target is a root element, refuse wrapping and show toast!
  const anyTargetIsARootElement = orderedActionTargets.some(EP.isRootElementOfInstance)
  if (anyTargetIsARootElement) {
    return showToast(notice("Root elements can't be wrapped into other elements", 'ERROR'))
  }

  const allTargetsCanBeGroupChildren = orderedActionTargets.every((path) => {
    return elementCanBeAGroupChild(
      MetadataUtils.getJsxElementChildFromMetadata(metadata, path),
      path,
      metadata,
    )
  })
  if (!allTargetsCanBeGroupChildren) {
    return showToast(notice('Not all targets can be wrapped into a Group', 'ERROR'))
  }

  const anyTargetIsAnEmptyGroup = orderedActionTargets.some((e) => isEmptyGroup(metadata, e))
  if (anyTargetIsAnEmptyGroup) {
    return showToast(notice('Empty Groups cannot be wrapped into Groups', 'ERROR'))
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

  const elementPropsOptic = fromTypeGuard<JSXElementLike, JSXElement>(isJSXElement).compose(
    fromField('props'),
  )

  function removeMarginsFromGroupChildElement(
    element: GroupChildElement,
    elementMetadata: ElementInstanceMetadata,
  ) {
    switch (element.type) {
      case 'JSX_ELEMENT':
      case 'JSX_FRAGMENT':
        // Remove any margin properties from the child.
        const elementLike = modify(elementPropsOptic, removeMarginProperties, element)
        return {
          element: elementLike,
          metadata: elementMetadata,
        }

      case 'JSX_CONDITIONAL_EXPRESSION':
        // Remove any margin properties from the branches.
        const conditionalWithUpdatedWhenTrue = modify(
          fromField<JSXConditionalExpression, 'whenTrue'>('whenTrue')
            .compose(fromTypeGuard(isJSXElement))
            .compose(fromField('props')),
          removeMarginProperties,
          element,
        )
        const conditional = modify(
          fromField<JSXConditionalExpression, 'whenFalse'>('whenFalse')
            .compose(fromTypeGuard(isJSXElement))
            .compose(fromField('props')),
          removeMarginProperties,
          conditionalWithUpdatedWhenTrue,
        )
        return {
          element: conditional,
          metadata: elementMetadata,
        }
      default:
        assertNever(element)
    }
  }

  const childComponents: Array<{
    element: GroupChildElement
    metadata: ElementInstanceMetadata
  }> = orderedActionTargets.map(
    (p): { element: GroupChildElement; metadata: ElementInstanceMetadata } => {
      const foundMetadata = MetadataUtils.findElementByElementPath(metadata, p)
      const element = foundMetadata?.element
      if (
        foundMetadata == null ||
        element == null ||
        isLeft(element) ||
        !elementCanBeAGroupChild(element.value, p, metadata)
      ) {
        throw new Error(
          `Invariant violation: ElementInstanceMetadata.element found for ${EP.toString(
            p,
          )} cannot be a group child`,
        )
      }
      return removeMarginsFromGroupChildElement(element.value, foundMetadata)
    },
  )

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
    generateUID(),
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

  const pinChangeCommands: ReadonlyArray<CanvasCommand> = arrayAccumulate((acc) => {
    orderedActionTargets.forEach((maybeTarget) => {
      return replaceFragmentLikePathsWithTheirChildrenRecursive(
        metadata,
        allElementProps,
        elementPathTrees,
        [maybeTarget],
      ).forEach((target) => {
        const expectedPathInsideGroup = forceNotNull(
          `invariant violation: no common path found between element and its descendants`,
          EP.replaceIfAncestor(target, EP.parentPath(maybeTarget), groupPath),
        )

        const foundMetadata = MetadataUtils.findElementByElementPath(metadata, target)

        acc.push(
          ...createPinChangeCommandsForElementBecomingGroupChild(
            metadata,
            foundMetadata,
            expectedPathInsideGroup,
            globalBoundingBoxOfAllElementsToBeWrapped,
            newLocalRectangleForGroup,
          ),
        )
      })
    })
  })

  const selectNewGroup = updateSelectedViews('always', [groupPath])

  return applyCommandsAction([
    ...deleteCommands,
    insertGroupCommand,
    ...pinChangeCommands,
    selectNewGroup,
    queueTrueUpElement([trueUpGroupElementChanged(groupPath)]),
  ])
}

export function createPinChangeCommandsForElementBecomingGroupChild(
  jsxMetadata: ElementInstanceMetadataMap,
  elementMetadata: ElementInstanceMetadata | null,
  expectedPath: ElementPath,
  globalBoundingBoxOfAllElementsToBeWrapped: CanvasRectangle,
  newLocalRectangleForGroup: LocalRectangle,
): Array<CanvasCommand> {
  if (
    elementMetadata == null ||
    isLeft(elementMetadata.element) ||
    !elementCanBeAGroupChild(
      elementMetadata.element.value,
      elementMetadata.elementPath,
      jsxMetadata,
    )
  ) {
    throw new Error(
      `Invariant violation: ElementInstanceMetadata.element found for ${EP.toString(
        expectedPath,
      )} cannot be a group child`,
    )
  }

  const childLocalRect: LocalRectangle = canvasRectangleToLocalRectangle(
    forceFiniteRectangle(elementMetadata.globalFrame),
    globalBoundingBoxOfAllElementsToBeWrapped,
  )
  switch (elementMetadata.element.value.type) {
    case 'JSX_ELEMENT':
      return commandsForPinChangeCommandForElementBecomingGroup(
        expectedPath,
        elementMetadata.element.value.props,
        childLocalRect,
        newLocalRectangleForGroup,
      )
    case 'JSX_CONDITIONAL_EXPRESSION':
      const activeBranch = getConditionalActiveCase(
        elementMetadata.elementPath,
        elementMetadata.element.value,
        jsxMetadata,
      )
      const branch =
        activeBranch === 'true-case'
          ? elementMetadata.element.value.whenTrue
          : elementMetadata.element.value.whenFalse
      return isJSXElement(branch)
        ? commandsForPinChangeCommandForElementBecomingGroup(
            expectedPath,
            branch.props,
            childLocalRect,
            newLocalRectangleForGroup,
          )
        : []
    case 'JSX_FRAGMENT':
      return []
    default:
      assertNever(elementMetadata.element.value)
  }
}

/**
 * This is an "optimistic" variant of createPinChangeCommandsForElementBecomingGroupChild
 * that will create missing pins for a new group child when its metadata is not available.
 * It will be using the existing pins of the element, if present, or default to either
 * the center of the group frame (or fallback 0,0) for its position and the parent width/height
 * for dimensions.
 */
export function createPinChangeCommandsForElementInsertedIntoGroup(
  expectedPath: ElementPath,
  props: PropsOrJSXAttributes,
  groupRectangle: CanvasRectangle,
  newLocalRectangleForGroup: LocalRectangle,
): Array<CanvasCommand> {
  function propOrZero(prop: StyleLayoutProp): number {
    const maybeProp = getLayoutProperty(prop, props, styleStringInArray)
    return isRight(maybeProp) && isCSSNumber(maybeProp.value) ? maybeProp.value.value : 0
  }
  let childLocalRect: LocalRectangle = localRectangle({
    x: propOrZero('left'),
    y: propOrZero('top'),
    width: propOrZero('width'),
    height: propOrZero('height'),
  })

  if (childLocalRect.width > 0) {
    childLocalRect.x = (groupRectangle.width - childLocalRect.width) / 2
  } else {
    childLocalRect.width = groupRectangle.width
  }
  if (childLocalRect.height > 0) {
    childLocalRect.y = (groupRectangle.height - childLocalRect.height) / 2
  } else {
    childLocalRect.height = groupRectangle.height
  }

  return commandsForPinChangeCommandForElementBecomingGroup(
    expectedPath,
    props.value,
    childLocalRect,
    newLocalRectangleForGroup,
  )
}

function commandsForPinChangeCommandForElementBecomingGroup(
  expectedPath: ElementPath,
  elementProps: JSXAttributes,
  childRect: LocalRectangle,
  groupRect: LocalRectangle,
): Array<CanvasCommand> {
  return [
    // make the child `position: absolute`
    setProperty('always', expectedPath, PP.create('style', 'position'), 'absolute'),
    // set child pins to match their intended new local rectangle
    ...setElementPinsForLocalRectangleEnsureTwoPinsPerDimension(
      expectedPath,
      elementProps,
      childRect,
      sizeFromRectangle(groupRect),
      null,
    ),
  ]
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

  function setPinPreserveHug(pin: 'width' | 'height', value: number): Array<SetCssLengthProperty> {
    const pinIsAlreadyHug = isHugFromStyleAttribute(elementCurrentProps, pin, 'only-max-content')

    if (pinIsAlreadyHug) {
      // we don't need to convert a Hug pin, do nothing here
      return []
    }
    return [setPin(pin, value)]
  }

  // TODO retarget Fragments
  const result = [
    setPin('left', localFrame.x),
    setPin('top', localFrame.y),
    setPin('right', parentSize.width - (localFrame.x + localFrame.width)),
    setPin('bottom', parentSize.height - (localFrame.y + localFrame.height)),
    ...setPinPreserveHug('width', localFrame.width),
    ...setPinPreserveHug('height', localFrame.height),
  ]
  return result
}

export function getCommandsForConversionToDesiredType(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  allElementProps: AllElementProps,
  selectedViews: Array<ElementPath>,
  currentType: EditorContract,
  desiredType: EditorContract,
): Array<CanvasCommand> {
  if (desiredType === 'wrapper-div') {
    throw new Error(
      'Invariant violation: wrapper-div should never be a selectable option in the dropdown',
    )
  }

  return selectedViews.flatMap((elementPath) => {
    if (currentType === 'fragment') {
      if (desiredType === 'fragment') {
        // NOOP
        return []
      }

      if (desiredType === 'frame') {
        return convertFragmentToFrame(
          metadata,
          elementPathTree,
          allElementProps,
          elementPath,
          'do-not-convert-if-it-has-static-children',
        )
      }

      if (desiredType === 'group') {
        return convertFragmentToGroup(metadata, elementPathTree, allElementProps, elementPath)
      }
      assertNever(desiredType)
    }

    if (currentType === 'frame' || currentType === 'wrapper-div') {
      if (desiredType === 'frame') {
        if (currentType === 'frame') {
          // NOOP
          return []
        }

        return convertWrapperToFrameCommands(
          metadata,
          allElementProps,
          elementPathTree,
          elementPath,
        )
      }

      if (desiredType === 'fragment') {
        return convertGroupOrFrameToFragmentCommands(
          metadata,
          elementPathTree,
          allElementProps,
          elementPath,
        )
      }

      if (desiredType === 'group') {
        return convertFrameToGroup(metadata, elementPathTree, allElementProps, elementPath)
      }
      assertNever(desiredType)
    }

    if (currentType === 'group') {
      if (desiredType === 'group') {
        // NOOP
        return []
      }

      if (desiredType === 'fragment') {
        return convertGroupOrFrameToFragmentCommands(
          metadata,
          elementPathTree,
          allElementProps,
          elementPath,
        )
      }

      if (desiredType === 'frame') {
        return convertGroupToFrameCommands(metadata, elementPathTree, allElementProps, elementPath)
      }
      assertNever(desiredType)
    }

    return assertNever(currentType)
  })
}
