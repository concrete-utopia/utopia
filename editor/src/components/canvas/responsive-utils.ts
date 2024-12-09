import type { ElementPath } from '../../core/shared/project-file-types'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

export function getContainingSceneWidth(
  elementPath: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): number | undefined {
  const containingScene = MetadataUtils.getParentSceneMetadata(jsxMetadata, elementPath)
  return containingScene?.specialSizeMeasurements?.clientWidth
}
