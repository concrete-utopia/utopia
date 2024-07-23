import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { is } from '../../../../core/shared/equality-utils'
import { memoize } from '../../../../core/shared/memoize'
import type { ElementPath } from '../../../../core/shared/project-file-types'
import type { AllElementProps } from '../../../editor/store/editor-state'
import type { InteractionCanvasState } from '../canvas-strategy-types'
import { getTargetPathsFromInteractionTarget } from '../canvas-strategy-types'
import { treatElementAsGroupLike, treatElementAsGroupLikeFromMetadata } from './group-helpers'
import { flattenSelection } from './shared-move-strategies-helpers'

type ReplacedPaths = { pathsWereReplaced: boolean; paths: Array<ElementPath> }

export function retargetStrategyToChildrenOfFragmentLikeElements(
  canvasState: InteractionCanvasState,
): ReplacedPaths {
  const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  const targetsWithoutDescedants = flattenSelection(targets)

  return replaceFragmentLikePathsWithTheirChildrenRecursiveFullReturnValue(
    canvasState.startingMetadata,
    canvasState.startingReconstructedDOMMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    targetsWithoutDescedants,
  )
}

// Return a list of paths for the children of all the given paths which are groups, unless the path is a group itself.
export function getChildGroupsForNonGroupParents(
  metadata: ElementInstanceMetadataMap,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  let result: Set<ElementPath> = new Set()
  for (const meta of Object.values(metadata)) {
    for (const path of paths) {
      if (EP.isDescendantOf(meta.elementPath, path) && treatElementAsGroupLikeFromMetadata(meta)) {
        result.add(meta.elementPath)
      }
    }
  }
  return Array.from(result)
}

export function retargetStrategyToTopMostFragmentLikeElement(
  canvasState: InteractionCanvasState,
): Array<ElementPath> {
  const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)
  const targetsWithoutDescedants = flattenSelection(targets)

  return optionallyReplacePathWithFragmentLikeParentRecursive(
    canvasState.startingMetadata,
    canvasState.startingReconstructedDOMMetadata,
    canvasState.startingAllElementProps,
    canvasState.startingElementPathTree,
    targetsWithoutDescedants,
  )
}

export function replaceFragmentLikePathsWithTheirChildrenRecursive(
  metadata: ElementInstanceMetadataMap,
  domReconstructedMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  return replaceFragmentLikePathsWithTheirChildrenRecursiveFullReturnValue(
    metadata,
    domReconstructedMetadata,
    allElementProps,
    pathTrees,
    paths,
  ).paths
}

const replaceFragmentLikePathsWithTheirChildrenRecursiveFullReturnValue = memoize(
  replaceFragmentLikePathsWithTheirChildrenRecursiveInner,
  { maxSize: 1, matchesArg: is },
)

function replaceFragmentLikePathsWithTheirChildrenRecursiveInner(
  metadata: ElementInstanceMetadataMap,
  domReconstructedMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  paths: Array<ElementPath>,
): ReplacedPaths {
  let pathsWereReplaced = false

  const updatedPaths = paths.flatMap((path) => {
    const elementIsFragmentLike = treatElementAsFragmentLike(
      metadata,
      domReconstructedMetadata,
      allElementProps,
      pathTrees,
      path,
    )

    if (elementIsFragmentLike) {
      const childrenAndUnfurled =
        MetadataUtils.getAllChildrenElementsIncludingUnfurledFocusedComponentsUnordered(path, {
          ...metadata,
          ...domReconstructedMetadata,
        })
      const children = [
        ...childrenAndUnfurled.children,
        ...childrenAndUnfurled.unfurledComponents,
      ].map((elementMetadata) => elementMetadata.elementPath)
      if (children.length === 0) {
        // with no children, actually let's just return the original element
        return path
      }

      pathsWereReplaced = true
      // Balazs: I think this is breaking the Memo!!!!!! this should be calling replaceFragmentLikePathsWithTheirChildrenRecursiveInner
      return replaceFragmentLikePathsWithTheirChildrenRecursive(
        metadata,
        domReconstructedMetadata,
        allElementProps,
        pathTrees,
        children,
      )
    }

    return path
  })

  return { pathsWereReplaced: pathsWereReplaced, paths: pathsWereReplaced ? updatedPaths : paths }
}

export const replaceNonDOMElementPathsWithTheirChildrenRecursive = memoize(
  replaceNonDOMElementPathsWithTheirChildrenRecursiveInner,
  { maxSize: 1, matchesArg: is },
)

function replaceNonDOMElementPathsWithTheirChildrenRecursiveInner(
  metadata: ElementInstanceMetadataMap,
  domReconstructedMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  let pathsWereReplaced = false

  const updatedPaths = paths.flatMap((path) => {
    const elementIsNonDOMElement = isElementNonDOMElement(
      metadata,
      domReconstructedMetadata,
      allElementProps,
      pathTrees,
      path,
    )

    if (elementIsNonDOMElement) {
      const children = MetadataUtils.getChildrenPathsOrdered(pathTrees, path)
      if (children.length === 0) {
        // with no children, actually let's just return the original element
        return path
      }

      pathsWereReplaced = true
      return replaceNonDOMElementPathsWithTheirChildrenRecursiveInner(
        metadata,
        domReconstructedMetadata,
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
  domReconstructedMetadata: ElementInstanceMetadataMap,
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
    !siblingPaths.every((t) =>
      treatElementAsFragmentLike(metadata, domReconstructedMetadata, allElementProps, pathTrees, t),
    )
  ) {
    return siblingPaths
  }

  const parent = EP.parentPath(siblingPaths[0])
  if (
    !treatElementAsFragmentLike(
      metadata,
      domReconstructedMetadata,
      allElementProps,
      pathTrees,
      parent,
    )
  ) {
    return siblingPaths
  }

  return optionallyReplacePathWithFragmentLikeParentRecursive(
    metadata,
    domReconstructedMetadata,
    allElementProps,
    pathTrees,
    [parent],
  )
}

export function replaceNonDomElementWithFirstDomAncestorPath(
  metadata: ElementInstanceMetadataMap,
  domReconstructedMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  maybeNonDomElement: ElementPath,
): ElementPath {
  if (
    treatElementAsFragmentLike(
      metadata,
      domReconstructedMetadata,
      allElementProps,
      pathTrees,
      maybeNonDomElement,
      'sizeless-div-not-considered-fragment-like',
    )
  ) {
    // if the element is fragment-like, try testing its parent path
    return replaceNonDomElementWithFirstDomAncestorPath(
      metadata,
      domReconstructedMetadata,
      allElementProps,
      pathTrees,
      EP.parentPath(maybeNonDomElement),
    )
  }

  // this path points to a dom element
  return maybeNonDomElement
}

export const AllFragmentLikeNonDomElementTypes = ['fragment', 'conditional'] as const
export const AllFragmentLikeTypes = [...AllFragmentLikeNonDomElementTypes, 'sizeless-div'] as const
export type FragmentLikeType = (typeof AllFragmentLikeTypes)[number] // <- this gives us the union type of the Array's entries

type SizelessDivsConsideredFragmentLike =
  | 'sizeless-div-considered-fragment-like'
  | 'sizeless-div-not-considered-fragment-like'

export function getElementFragmentLikeType(
  metadata: ElementInstanceMetadataMap,
  domReconstructedMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  path: ElementPath,
  sizelessDivsConsideredFragmentLike: SizelessDivsConsideredFragmentLike = 'sizeless-div-considered-fragment-like',
): FragmentLikeType | null {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)

  const elementProps = allElementProps[EP.toString(path)]

  if (treatElementAsGroupLike(metadata, path)) {
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

  const children = MetadataUtils.getChildrenOrderedMultiMetadata(
    [metadata, domReconstructedMetadata],
    pathTrees,
    path,
  )
  const childrenCount = children.length
  if (childrenCount === 0) {
    // do not treat elements with zero children as fragment-like
    return null
  }

  const hasNoWidthAndHeightProps = // TODO make this information come from the computed style instead of reading the style prop
    elementProps?.['style']?.['width'] == null && elementProps?.['style']?.['height'] == null

  const allChildrenAreAbsolute = children.every(MetadataUtils.isPositionAbsolute)

  if (
    sizelessDivsConsideredFragmentLike === 'sizeless-div-considered-fragment-like' &&
    hasNoWidthAndHeightProps &&
    allChildrenAreAbsolute
  ) {
    return 'sizeless-div'
  }

  return null
}

export function treatElementAsFragmentLike(
  metadata: ElementInstanceMetadataMap,
  domReconstructedMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  path: ElementPath,
  sizelessDivsConsideredFragmentLike: SizelessDivsConsideredFragmentLike = 'sizeless-div-considered-fragment-like',
): boolean {
  return (
    getElementFragmentLikeType(
      metadata,
      domReconstructedMetadata,
      allElementProps,
      pathTrees,
      path,
      sizelessDivsConsideredFragmentLike,
    ) != null
  )
}

export function isElementNonDOMElement(
  metadata: ElementInstanceMetadataMap,
  domReconstructedMetadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  pathTrees: ElementPathTrees,
  elementPath: ElementPath,
): boolean {
  const fragmentLikeType = getElementFragmentLikeType(
    metadata,
    domReconstructedMetadata,
    allElementProps,
    pathTrees,
    elementPath,
  )
  return AllFragmentLikeNonDomElementTypes.some((type) => fragmentLikeType === type)
}
