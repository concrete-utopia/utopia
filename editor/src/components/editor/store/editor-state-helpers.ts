import { createSelector, createSelectorCreator } from 'reselect'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { getElementWarnings } from './editor-state'
import type {
  MetadataAndPropertyControlsInfoSubstate,
  MetadataSubstate,
  ProjectContentAndMetadataSubstate,
} from './store-hook-substore-types'
import { shallowEqual } from '../../../core/shared/equality-utils'

export const getElementWarningsSelector = createSelector(
  (store: ProjectContentAndMetadataSubstate) => store.editor.projectContents,
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate) => store.editor.allElementProps,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  (projectContents, jsxMetadata, allElementProps, elementPathTree) => {
    return getElementWarnings(projectContents, jsxMetadata, allElementProps, elementPathTree)
  },
)

export const getAutofocusedPathsSelector = createSelector(
  (store: MetadataAndPropertyControlsInfoSubstate) => store.editor.projectContents,
  (store: MetadataSubstate) => store.editor.jsxMetadata,
  (store: MetadataSubstate) => store.editor.elementPathTree,
  (store: MetadataAndPropertyControlsInfoSubstate) => store.editor.propertyControlsInfo,
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
  { memoizeOptions: { resultEqualityCheck: shallowEqual } },
)
