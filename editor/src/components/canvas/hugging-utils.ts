import { MetadataUtils } from '../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import type { ElementPath } from '../../core/shared/project-file-types'

export type HuggingElementContentsStatus =
  | 'empty'
  | 'contains-only-absolute'
  | 'contains-some-absolute'
  | 'non-empty'

export function getHuggingElementContentsStatus(
  jsxMetadata: ElementInstanceMetadataMap,
  path: ElementPath,
): HuggingElementContentsStatus {
  const children = MetadataUtils.getChildrenUnordered(jsxMetadata, path)
  const absoluteChildren = children.filter(MetadataUtils.isPositionAbsolute).length
  if (children.length === 0) {
    return 'empty'
  } else if (absoluteChildren === children.length) {
    return 'contains-only-absolute'
  } else if (absoluteChildren > 0) {
    return 'contains-some-absolute'
  } else {
    return 'non-empty'
  }
}
