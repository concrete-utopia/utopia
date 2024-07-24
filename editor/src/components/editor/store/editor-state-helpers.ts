import { createSelector } from 'reselect'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getElementWarnings } from './editor-state'
import type {
  MetadataSubstate,
  ProjectContentSubstate,
  RestOfEditorState,
} from './store-hook-substore-types'

export const getElementWarningsSelector = createSelector(
  (store: ProjectContentSubstate) => store.editor.projectContents,
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  (projectContents, jsxMetadata, allElementProps, elementPathTree) => {
    return getElementWarnings(projectContents, jsxMetadata, allElementProps, elementPathTree)
  },
)

export const getAutofocusedPathsSelector = createSelector(
  (store: ProjectContentSubstate) => store.editor.projectContents,
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  (store: RestOfEditorState) => store.editor.propertyControlsInfo,
  (projectContents, jsxMetadata, elementPathTree, propertyControlsInfo) => {
    return MetadataUtils.getAllPaths(jsxMetadata, elementPathTree).filter((path) => {
      return MetadataUtils.isAutofocusable(
        jsxMetadata,
        elementPathTree,
        path,
        propertyControlsInfo,
        projectContents,
      )
    })
  },
)
