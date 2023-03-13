import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { foldEither } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
  isJSXFragment,
} from '../../../../core/shared/element-template'
import { is } from '../../../../core/shared/equality-utils'
import { memoize } from '../../../../core/shared/memoize'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { AllElementProps } from '../../../editor/store/editor-state'
import {
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { getDragTargets } from './shared-move-strategies-helpers'

export function retargetStrategyToChildrenOfContentAffectingElements(
  canvasState: InteractionCanvasState,
): Array<ElementPath> {
  const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  const targetsWithoutDescedants = getDragTargets(targets)

  return replaceContentAffectingPathsWithTheirChildrenRecursive(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    targetsWithoutDescedants,
  )
}

export const replaceContentAffectingPathsWithTheirChildrenRecursive = memoize(
  replaceContentAffectingPathsWithTheirChildrenRecursiveInner,
  { maxSize: 1, equals: is },
)

function replaceContentAffectingPathsWithTheirChildrenRecursiveInner(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  let pathsWereReplaced = false

  const updatedPaths = paths.flatMap((path) => {
    const elementIsContentAffecting = treatElementAsContentAffecting(
      metadata,
      allElementProps,
      path,
    )

    if (elementIsContentAffecting) {
      const children = MetadataUtils.getChildrenPathsUnordered(metadata, path) // I think it's fine to get the unordered children here?
      if (children.length === 0) {
        // with no children, actually let's just return the original element
        return path
      }

      pathsWereReplaced = true
      return replaceContentAffectingPathsWithTheirChildrenRecursive(
        metadata,
        allElementProps,
        children,
      )
    }

    return path
  })

  return pathsWereReplaced ? updatedPaths : paths
}

type ContentAffectingType = 'fragment' | 'conditional' | 'sizeless-div'

export function getElementContentAffectingType(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  path: ElementPath,
): ContentAffectingType | null {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)

  const elementProps = allElementProps[EP.toString(path)]

  if (
    elementMetadata?.element != null &&
    foldEither(
      () => false,
      (e) => isJSXFragment(e),
      elementMetadata.element,
    )
  ) {
    return 'fragment'
  }

  if (
    elementMetadata?.element != null &&
    foldEither(
      () => false,
      (e) => isJSXConditionalExpression(e),
      elementMetadata.element,
    )
  ) {
    return 'conditional'
  }

  if (MetadataUtils.isFlexLayoutedContainer(elementMetadata)) {
    // for now, do not treat flex parents ever as content-affecting / group-like
    return null
  }

  if (EP.isStoryboardPath(path)) {
    // the Storyboard is not children-affecting
    return null
  }

  const childrenCount = MetadataUtils.getChildrenUnordered(metadata, path).length
  if (childrenCount === 0) {
    // do not treat elements with zero children as content-affecting
    return null
  }

  const hasNoWidthAndHeightProps =
    elementProps?.['style']?.['width'] == null && elementProps?.['style']?.['height'] == null

  if (hasNoWidthAndHeightProps) {
    return 'sizeless-div'
  }

  return null
}

export function treatElementAsContentAffecting(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  path: ElementPath,
): boolean {
  const type = getElementContentAffectingType(metadata, allElementProps, path)
  switch (type) {
    case null:
    case 'conditional':
      return false
    default:
      return true
  }
}
