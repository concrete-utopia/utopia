import { ElementPath } from '../../core/shared/project-file-types'
import * as EP from '../../core/shared/element-path'
import { isFeatureEnabled } from '../../utils/feature-switches'
import {
  ElementInstanceMetadataMap,
  isJSXConditionalExpression,
  isJSXFragment,
} from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { foldEither } from '../../core/shared/either'
import {
  isConditionalClauseNavigatorEntry,
  NavigatorEntry,
  navigatorEntryToKey,
} from '../editor/store/editor-state'

export function navigatorDepth(path: ElementPath): number {
  // The storyboard means that this starts at -1,
  // so that the scenes are the left most entity.
  let result: number = -1
  for (const pathPart of path.parts) {
    result = result + pathPart.length
  }
  return result
}

export function navigatorDepthAncestorShift(
  navigatorEntry: NavigatorEntry,
  metadata: ElementInstanceMetadataMap,
): number {
  const path = navigatorEntry.elementPath
  let result: number = 0
  for (const ancestorPath of EP.getAncestors(path)) {
    const elementMetadata = MetadataUtils.findElementByElementPath(metadata, ancestorPath)
    if (elementMetadata != null) {
      // If fragments are not supported, shift the depth back by 1 as they will not be included in the
      // hierarchy.
      if (!isFeatureEnabled('Fragment support')) {
        const isFragment = foldEither(
          () => false,
          (e) => isJSXFragment(e),
          elementMetadata.element,
        )
        if (isFragment) {
          result = result - 1
        }
      }

      // A conditional ancestor will shift this by an additional 1, for the clause.
      if (isFeatureEnabled('Conditional support')) {
        const isConditional = foldEither(
          () => false,
          (e) => isJSXConditionalExpression(e),
          elementMetadata.element,
        )
        if (isConditional) {
          result = result + 1
        }
      }
    }
  }

  // For the clause entry itself, this needs to step back by 1.
  if (
    isFeatureEnabled('Conditional support') &&
    isConditionalClauseNavigatorEntry(navigatorEntry)
  ) {
    result = result - 1
  }

  return result
}
