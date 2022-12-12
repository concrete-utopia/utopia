import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { EditorStorePatched } from '../editor/store/editor-state'

export const metadataSelector = (store: EditorStorePatched): ElementInstanceMetadataMap =>
  store.editor.jsxMetadata

export const selectedViewsSelector = (store: EditorStorePatched): ElementPath[] =>
  store.editor.selectedViews
