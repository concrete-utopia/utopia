import createCachedSelector from 're-reselect'
import type { MetadataSubstate, ProjectContentSubstate } from './store-hook-substore-types'
import { getElementWarnings } from './editor-state'

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
