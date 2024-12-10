import type { ElementPath } from '../../core/shared/project-file-types'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { noSceneSize, sceneSize, type SceneSize } from './plugins/style-plugins'
import type { EditorState } from '../editor/store/editor-state'

export function getContainingSceneSize(
  selectedElement: ElementPath,
  jsxMetadata: ElementInstanceMetadataMap,
): SceneSize {
  if (selectedElement == null) {
    return noSceneSize()
  }
  const containingScene = MetadataUtils.getParentSceneMetadata(jsxMetadata, selectedElement)
  return sceneSize(containingScene?.specialSizeMeasurements?.clientWidth)
}

export function getContainingSceneSizeFromEditorState(editor: EditorState): SceneSize {
  if (editor == null) {
    return noSceneSize()
  }
  // we're taking the first selected element because we're assuming elements are in the same scene
  // TODO: support multiple selected elements that are in different scenes?
  const selectedElement = editor.selectedViews.at(0)
  if (selectedElement == null) {
    return noSceneSize()
  }
  return getContainingSceneSize(selectedElement, editor.jsxMetadata)
}
