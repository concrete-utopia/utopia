import { createSelector } from 'reselect'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { ElementPath } from '../../core/shared/project-file-types'
import { EditorStorePatched } from '../editor/store/editor-state'
import { DefaultFlexDirection, detectFlexDirection } from './inspector-common'

export const metadataSelector = (store: EditorStorePatched): ElementInstanceMetadataMap =>
  store.editor.jsxMetadata

export const selectedViewsSelector = (store: EditorStorePatched): ElementPath[] =>
  store.editor.selectedViews

export const flexDirectionSelector = createSelector(
  metadataSelector,
  selectedViewsSelector,
  (metadata, selectedViews) =>
    selectedViews.length === 0
      ? DefaultFlexDirection
      : detectFlexDirection(metadata, selectedViews),
)
