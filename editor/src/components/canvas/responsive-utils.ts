import type { ElementPath } from '../../core/shared/project-file-types'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { noSceneSize, sceneSize, type SceneSize } from './plugins/style-plugins'

export function getContainingSceneSize({
  selectedViews,
  jsxMetadata,
}: {
  selectedViews: ElementPath[]
  jsxMetadata: ElementInstanceMetadataMap
}): SceneSize {
  // TODO: support multiple selected elements in different scenes?
  const selectedElement = selectedViews.at(0)
  if (selectedElement == null) {
    return noSceneSize()
  }
  const containingScene = MetadataUtils.getParentSceneMetadata(jsxMetadata, selectedElement)
  return sceneSize(containingScene?.specialSizeMeasurements?.clientWidth)
}
