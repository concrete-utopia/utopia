import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import {
  EditorStorePatched,
  MetadataSubstate,
  SelectedHighlightedViewsSubstate,
} from '../editor/store/editor-state'

export const metadataSelector = (store: MetadataSubstate): ElementInstanceMetadataMap =>
  store.editor.jsxMetadata

export const selectedViewsSelector = (store: {
  editor: { selectedViews: ElementPath[] }
}): ElementPath[] => store.editor.selectedViews
