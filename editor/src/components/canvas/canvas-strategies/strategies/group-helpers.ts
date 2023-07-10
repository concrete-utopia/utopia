import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { ElementInstanceMetadataMap } from '../../../../core/shared/element-template'
import type { ElementPath } from '../../../../core/shared/project-file-types'

export function treatElementAsGroupLike(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  path: ElementPath,
): boolean {
  const allChildrenAreAbsolute = MetadataUtils.getChildrenOrdered(metadata, pathTrees, path).every(
    (child) => child.specialSizeMeasurements.position === 'absolute',
  )
  return (
    allChildrenAreAbsolute &&
    MetadataUtils.isGroupAgainstImports(MetadataUtils.findElementByElementPath(metadata, path))
  )
}
