import createCachedSelector from 're-reselect'
import type {
  MetadataSubstate,
  ProjectContentSubstate,
  RestOfEditorState,
} from './store-hook-substore-types'
import { getElementWarnings } from './editor-state'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'

export const getElementWarningsSelector = createCachedSelector(
  (store: ProjectContentSubstate, storeType: 'patched' | 'unpatched') =>
    store.editor.projectContents,
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  (projectContents, jsxMetadata, allElementProps, elementPathTree) => {
    return getElementWarnings(projectContents, jsxMetadata, allElementProps, elementPathTree)
  },
)((store, storeType: 'patched' | 'unpatched') => storeType)

export const getAutofocusedPathsSelector = createCachedSelector(
  (store: ProjectContentSubstate, storeType: 'patched' | 'unpatched') =>
    store.editor.projectContents,
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
)((store, storeType: 'patched' | 'unpatched') => storeType)
