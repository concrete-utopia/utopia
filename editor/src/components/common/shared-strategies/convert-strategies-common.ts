import type { ElementPath } from 'utopia-shared/src/types'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementPathTrees } from '../../../core/shared/element-path-tree'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import {
  isElementNonDOMElement,
  replaceNonDOMElementPathsWithTheirChildrenRecursive,
} from '../../canvas/canvas-strategies/strategies/fragment-like-helpers'
import type { AllElementProps } from '../../editor/store/editor-state'

export type FlexDirectionRowColumn = 'row' | 'column' // a limited subset as we never guess row-reverse or column-reverse
export type FlexAlignItems = 'center' | 'flex-end'

export function getChildrenPathsForContainer(
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  path: ElementPath,
  allElementProps: AllElementProps,
) {
  return MetadataUtils.getChildrenPathsOrdered(metadata, elementPathTree, path).flatMap((child) =>
    isElementNonDOMElement(metadata, allElementProps, elementPathTree, child)
      ? replaceNonDOMElementPathsWithTheirChildrenRecursive(
          metadata,
          allElementProps,
          elementPathTree,
          [child],
        )
      : child,
  )
}
