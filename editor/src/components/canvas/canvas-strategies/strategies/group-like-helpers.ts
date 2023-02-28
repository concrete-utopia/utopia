import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { foldEither } from '../../../../core/shared/either'
import * as EP from '../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
  isJSXElement,
  isJSXFragment,
  isJSXTextBlock,
} from '../../../../core/shared/element-template'
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

function isNonDomElement(metadata: ElementInstanceMetadataMap, path: ElementPath): boolean {
  const instance = MetadataUtils.findElementByElementPath(metadata, path)
  return (
    instance != null &&
    foldEither(
      () => false,
      (e) => isJSXFragment(e) || isJSXConditionalExpression(e),
      instance.element,
    )
  )
}

export function replaceNonDomElementPathsWithTheirChildrenRecursive(
  metadata: ElementInstanceMetadataMap,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  return paths.flatMap((path) => {
    if (isNonDomElement(metadata, path)) {
      return replaceNonDomElementPathsWithTheirChildrenRecursive(
        metadata,
        MetadataUtils.getChildrenPathsUnordered(metadata, path),
      )
    }
    return [path]
  })
}

export function replaceContentAffectingPathsWithTheirChildrenRecursive(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  return paths.flatMap((path) => {
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
      return replaceContentAffectingPathsWithTheirChildrenRecursive(
        metadata,
        allElementProps,
        children,
      )
    }

    return path
  })
}

// TODO make it internal
export function treatElementAsContentAffecting(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  path: ElementPath,
): boolean {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)

  const elementProps = allElementProps[EP.toString(path)]

  if (MetadataUtils.isFlexLayoutedContainer(elementMetadata)) {
    // for now, do not treat flex parents ever as content-affecting / group-like
    return false
  }

  if (EP.isStoryboardPath(path)) {
    // the Storyboard is not children-affecting
    return false
  }

  const childrenCount = MetadataUtils.getChildrenUnordered(metadata, path).length
  if (childrenCount === 0) {
    // do not treat elements with zero children as content-affecting
    return false
  }

  const hasNoWidthAndHeightProps =
    elementProps?.['style']?.['width'] == null && elementProps?.['style']?.['height'] == null

  return hasNoWidthAndHeightProps
}
