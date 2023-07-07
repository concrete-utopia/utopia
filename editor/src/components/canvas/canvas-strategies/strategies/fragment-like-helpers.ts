import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import {
  getSimpleAttributeAtPath,
  MetadataUtils,
} from '../../../../core/model/element-metadata-utils'
import { findUtopiaCommentFlag } from '../../../../core/shared/comment-flags'
import { foldEither, isLeft, right } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { isJSXConditionalExpression, isJSXElement } from '../../../../core/shared/element-template'
import { is } from '../../../../core/shared/equality-utils'
import { memoize } from '../../../../core/shared/memoize'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import * as PP from '../../../../core/shared/property-path'
import type { AllElementProps } from '../../../editor/store/editor-state'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import { getTargetPathsFromInteractionTarget } from '../canvas-strategy-types'
import { treatElementAsGroupLike } from './group-helpers'
import { flattenSelection } from './shared-move-strategies-helpers'

export function retargetStrategyToChildrenOfFragmentLikeElements(
  canvasState: InteractionCanvasState,
): Array<ElementPath> {
  const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  const targetsWithoutDescedants = flattenSelection(targets)

  return replaceFragmentLikePathsWithTheirChildrenRecursive(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    targetsWithoutDescedants,
  )
}

export function retargetStrategyToTopMostFragmentLikeElement(
  canvasState: InteractionCanvasState,
): Array<ElementPath> {
  const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const targetsWithoutDescedants = flattenSelection(targets)

  return optionallyReplacePathWithFragmentLikeParentRecursive(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    targetsWithoutDescedants,
  )
}

export const replaceFragmentLikePathsWithTheirChildrenRecursive = memoize(
  replaceFragmentLikePathsWithTheirChildrenRecursiveInner,
  { maxSize: 1, equals: is },
)

function replaceFragmentLikePathsWithTheirChildrenRecursiveInner(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  let pathsWereReplaced = false

  const updatedPaths = paths.flatMap((path) => {
    const elementIsFragmentLike = treatElementAsFragmentLike(
      metadata,
      allElementProps,
      pathTrees,
      path,
    )

    if (elementIsFragmentLike) {
      const children = MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, path)
      if (children.length === 0) {
        // with no children, actually let's just return the original element
        return path
      }

      pathsWereReplaced = true
      // Balazs: I think this is breaking the Memo!!!!!! this should be calling replaceFragmentLikePathsWithTheirChildrenRecursiveInner
      return replaceFragmentLikePathsWithTheirChildrenRecursive(
        metadata,
        allElementProps,
        pathTrees,
        children,
      )
    }

    return path
  })

  return pathsWereReplaced ? updatedPaths : paths
}

export const replaceNonDOMElementPathsWithTheirChildrenRecursive = memoize(
  replaceNonDOMElementPathsWithTheirChildrenRecursiveInner,
  { maxSize: 1, equals: is },
)

function replaceNonDOMElementPathsWithTheirChildrenRecursiveInner(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  let pathsWereReplaced = false

  const updatedPaths = paths.flatMap((path) => {
    const elementIsNonDOMElement = isElementNonDOMElement(
      metadata,
      allElementProps,
      pathTrees,
      path,
    )

    if (elementIsNonDOMElement) {
      const children = MetadataUtils.getChildrenPathsOrdered(metadata, pathTrees, path)
      if (children.length === 0) {
        // with no children, actually let's just return the original element
        return path
      }

      pathsWereReplaced = true
      return replaceNonDOMElementPathsWithTheirChildrenRecursiveInner(
        metadata,
        allElementProps,
        pathTrees,
        children,
      )
    }

    return path
  })

  return pathsWereReplaced ? updatedPaths : paths
}

export function optionallyReplacePathWithFragmentLikeParentRecursive(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  siblingPaths: Array<ElementPath>,
): Array<ElementPath> {
  if (siblingPaths.length === 0) {
    return siblingPaths
  }

  if (!EP.areSiblings(siblingPaths)) {
    return siblingPaths
  }

  if (
    !siblingPaths.every((t) => treatElementAsFragmentLike(metadata, allElementProps, pathTrees, t))
  ) {
    return siblingPaths
  }

  const parent = EP.parentPath(siblingPaths[0])
  if (!treatElementAsFragmentLike(metadata, allElementProps, pathTrees, parent)) {
    return siblingPaths
  }

  return optionallyReplacePathWithFragmentLikeParentRecursive(
    metadata,
    allElementProps,
    pathTrees,
    [parent],
  )
}

export const AllFragmentLikeNonDomElementTypes = ['fragment', 'conditional'] as const
export const AllFragmentLikeTypes = [...AllFragmentLikeNonDomElementTypes, 'sizeless-div'] as const
export type FragmentLikeType = typeof AllFragmentLikeTypes[number] // <- this gives us the union type of the Array's entries

export function getElementFragmentLikeType(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): FragmentLikeType | null {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)

  const elementProps = allElementProps[EP.toString(path)]

  if (treatElementAsGroupLike(metadata, pathTrees, path)) {
    // to ensure mutual exclusivity
    return null
  }

  if (MetadataUtils.isFragmentFromMetadata(elementMetadata)) {
    return 'fragment'
  }

  if (MetadataUtils.isConditionalFromMetadata(elementMetadata)) {
    return 'conditional'
  }

  if (MetadataUtils.isFlexLayoutedContainer(elementMetadata)) {
    // for now, do not treat flex parents ever as fragment-like
    return null
  }

  if (EP.isStoryboardPath(path)) {
    // the Storyboard is not fragment-like
    return null
  }

  const children = MetadataUtils.getChildrenOrdered(metadata, pathTrees, path)
  const childrenCount = children.length
  if (childrenCount === 0) {
    // do not treat elements with zero children as fragment-like
    return null
  }

  const hasNoWidthAndHeightProps =
    elementProps?.['style']?.['width'] == null && elementProps?.['style']?.['height'] == null

  const allChildrenAreAbsolute = children.every(MetadataUtils.isPositionAbsolute)

  if (hasNoWidthAndHeightProps && allChildrenAreAbsolute) {
    return 'sizeless-div'
  }

  return null
}

export function treatElementAsFragmentLike(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): boolean {
  return getElementFragmentLikeType(metadata, allElementProps, pathTrees, path) != null
}

export function isElementNonDOMElement(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): boolean {
  const fragmentLikeType = getElementFragmentLikeType(
    metadata,
    allElementProps,
    pathTrees,
    elementPath,
  )
  return AllFragmentLikeNonDomElementTypes.some((type) => fragmentLikeType === type)
}
