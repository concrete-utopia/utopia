import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import { ElementPath } from '../../../../core/shared/project-file-types'

export function treatElementAsGroupLike(
  metadata: ElementInstanceMetadataMap,
  path: ElementPath,
): boolean {
  return MetadataUtils.isGroupAgainstImports(MetadataUtils.findElementByElementPath(metadata, path))
}
