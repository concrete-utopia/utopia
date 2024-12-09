import type { ElementPath } from '../../core/shared/project-file-types'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

export function getContainingSceneWidth({
  selectedViews,
  jsxMetadata,
}: {
  selectedViews: ElementPath[]
  jsxMetadata: ElementInstanceMetadataMap
}): number | undefined {
  // TODO: support multiple selected elements in different scenes?
  const selectedElement = selectedViews.at(0)
  if (selectedElement == null) {
    return undefined
  }
  const containingScene = MetadataUtils.getParentSceneMetadata(jsxMetadata, selectedElement)
  return containingScene?.specialSizeMeasurements?.clientWidth
}
