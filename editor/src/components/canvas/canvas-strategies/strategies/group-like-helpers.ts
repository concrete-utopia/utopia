import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import * as EP from '../../../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'
import { AllElementProps } from '../../../editor/store/editor-state'
import {
  getTargetPathsFromInteractionTarget,
  InteractionCanvasState,
} from '../canvas-strategy-types'
import { getDragTargets } from './shared-move-strategies-helpers'

export function retargetStrategyToChildrenOfGroupLike(
  canvasState: InteractionCanvasState,
): Array<ElementPath> {
  const targets = getTargetPathsFromInteractionTarget(canvasState.interactionTarget)

  const targetsWithoutDescedants = getDragTargets(targets)

  return replaceGroupLikePathsWithTheirChildrenRecursive(
    canvasState.startingMetadata,
    canvasState.startingAllElementProps,
    targetsWithoutDescedants,
  )
}

function replaceGroupLikePathsWithTheirChildrenRecursive(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  paths: Array<ElementPath>,
): Array<ElementPath> {
  return paths.flatMap((path) => {
    const elementIsGroupLike = treatElementAsGroupLike(metadata, allElementProps, path)

    if (elementIsGroupLike) {
      const children = MetadataUtils.getChildrenPaths(metadata, path)
      return replaceGroupLikePathsWithTheirChildrenRecursive(metadata, allElementProps, children)
    }

    return path
  })
}

// TODO make it internal
export function treatElementAsGroupLike(
  metadata: ElementInstanceMetadataMap,
  allElementProps: AllElementProps,
  path: ElementPath,
): boolean {
  const elementMetadata = MetadataUtils.findElementByElementPath(metadata, path)

  const elementProps = allElementProps[EP.toString(path)]

  const hasNoWidthAndHeightProps =
    elementProps?.['style']?.['width'] == null && elementProps?.['style']?.['height'] == null

  return hasNoWidthAndHeightProps
}
